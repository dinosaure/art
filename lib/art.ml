[@@@landmark "auto"]

module String = struct
  include Stdlib.String

  external unsafe_get_uint32 : string -> int -> int32 = "%caml_string_get32"
end

external ( <= ) : 'a -> 'a -> bool = "%lessequal"
let ( <= ) (x : int) y = x <= y [@@inline]
let min (a : int) b = if a <= b then a else b [@@inline]

let ( .!{} ) = Bytes.unsafe_get
let ( .!{}<- ) = Bytes.unsafe_set

type key = string (* + \000 *)

let ( .![] ) str i = String.unsafe_get str i

type 'a kind =
  | N4   : n4   kind
  | N16  : n16  kind
  | N48  : n48  kind
  | N256 : n256 kind
  | NULL : unit kind
and n4 = bytes
(* XXX(dinosaure): [Obj.(size (repr "\000\000\000\000"))] < [Obj.(size (repr { _= 0; _= 0; }))] *)
and n16 = bytes
and n48 = bytes
and n256 = N256_Key

type 'a record =
  { prefix : bytes
  ; mutable prefix_length : int
  ; mutable count : int
  ; kind : 'a kind
  ; keys : 'a }

type 'a node =
  { header : header
  ; children : 'a elt array }
and 'a leaf =
  { value : 'a
  ; key : key }
and 'a elt =
  | Leaf of 'a leaf
  | Node of 'a node
and header = Header : 'a record -> header [@@unboxed]

let key : string -> key = fun key ->
  if String.contains key '\000' then invalid_arg "Invalid key" ; key

external unsafe_key : string -> key = "%identity"

type 'a t = 'a elt ref

let[@coverage off] pp_char ppf = function
  | '\x21' .. '\x7e' as chr -> Fmt.char ppf chr
  | chr -> Fmt.pf ppf "%02x" (Char.code chr)

let[@coverage off] pp_n4 ppf keys =
  Fmt.pf ppf "%a"
    Fmt.(Dump.array pp_char)
    (Array.init 4 (fun i -> keys.!{i}))

let[@coverage off] pp_n16 ppf keys =
  Fmt.pf ppf "%a"
    Fmt.(Dump.array pp_char)
    (Array.init 16 (fun i -> keys.!{i}))

let[@coverage off] pp_n48 ppf keys =
  Fmt.pf ppf "%a" Fmt.(Dump.array pp_char) (Array.init 16 (fun i -> keys.!{i}))

let[@coverage off] pp_n256 _ppf N256_Key = ()

let[@coverage off] pp_keys : type a. kind:a kind -> a Fmt.t = fun ~kind -> match kind with
  | N4 -> pp_n4
  | N16 -> pp_n16
  | N48 -> pp_n48
  | N256 -> pp_n256
  | NULL -> Fmt.nop

let[@coverage off] pp_kind : type a. a kind Fmt.t = fun ppf -> function
  | N4 -> Fmt.string ppf "N4"
  | N16 -> Fmt.string ppf "N16"
  | N48 -> Fmt.string ppf "N48"
  | N256 -> Fmt.string ppf "N256"
  | NULL -> Fmt.string ppf "NULL"

let[@coverage off] pp_record : type a. a record Fmt.t = fun ppf r ->
  match r.kind with
  | NULL -> Fmt.string ppf "<null>"
  | _ ->
    Fmt.pf ppf "{ @[<hov>prefix= %S;@ \
                         prefix_length= %d;@ \
                         count= %d;@ \
                         kind= %a;@ \
                         value= @[<hov>%a@];@] }"
      (Bytes.unsafe_to_string r.prefix) r.prefix_length
      r.count pp_kind r.kind
      (pp_keys ~kind:r.kind) r.keys

let[@coverage off] pp_header ppf (Header record) = pp_record ppf record

let[@coverage off] rec pp_elt pp_value ppf = function
  | Leaf { key; value; } ->
    Fmt.pf ppf "{:leaf @[<hov>key= %S;@ value= @[<hov>%a@];@] }" key pp_value value
  | Node { header= Header { kind= NULL; _ }; _ } ->
    Fmt.string ppf "<null>"
  | Node { header; children; } ->
    Fmt.pf ppf "{:node @[<hov>hdr= @[<hov>%a@];@ children= @[<hov>%a@];@] }"
      pp_header header Fmt.(Dump.array (pp_elt pp_value)) children

let[@coverage off] pp pp_value ppf tree = pp_elt pp_value ppf !tree

external ctz : int -> int = "caml_ctz" [@@noalloc]

let empty_record =
  { prefix= Bytes.empty; prefix_length= 0
  ; count= 0
  ; kind= NULL; keys= () }

let empty_header = Header empty_record
let empty_node = { header= empty_header; children= [||] }
let empty_elt = Node empty_node

let n4 () : n4 record =
  let prefix = Bytes.make 10 '\000' in
  let record =
    { prefix; prefix_length= 0;
      count= 0;
      kind= N4; keys= Bytes.make 4 '\000' } in
  record

let n4_shift n4 = function
  | 0 ->
    n4.!{3} <- n4.!{2}
  ; n4.!{2} <- n4.!{1}
  ; n4.!{1} <- n4.!{0}
  | 1 ->
    n4.!{3} <- n4.!{2}
  ; n4.!{2} <- n4.!{1}
  | _ (* 2 *) ->
    n4.!{3} <- n4.!{2}

let n16 prefix : n16 record =
  let record =
    { prefix; prefix_length= 0;
      count= 0;
      kind= N16; keys= Bytes.make 16 '\000' } in
  record

let n16_shift keys n =
  Bytes.unsafe_blit keys n keys (n + 1) (16 - (n + 1))

let n48 prefix : n48 record =
  let keys = Bytes.make 256 '\048' in
  let record =
    { prefix; prefix_length= 0;
      count= 0;
      kind= N48; keys; } in
  record

let n256 prefix : n256 record =
  let record =
    { prefix; prefix_length= 0;
      count= 0;
      kind= N256; keys= N256_Key; } in
  record

let memcmp a b ~off ~len =
  let len0 = len land 3 in
  let len1 = len asr 2 in

  for i = 0 to len1 - 1 do
    let i = off + i * 4 in
    if String.unsafe_get_uint32 a i <> String.unsafe_get_uint32 b i
    then raise_notrace Not_found ;
  done ;

  for i = 0 to len0 - 1 do
    let i = off + len1 * 4 + i in
    if a.![i] <> b.![i] then raise_notrace Not_found ;
  done
;;

let copy_header : type a b. src:a record -> dst:b record -> unit = fun ~src ~dst ->
  dst.count <- src.count ;
  dst.prefix_length <- src.prefix_length

let add_child_n256
  : n256 record -> 'a elt array -> char -> 'a elt -> unit
  = fun record children chr node ->
    record.count <- record.count + 1 ;
    Array.unsafe_set children (Char.code chr) node

let add_child_n48
  : n48 record -> 'a elt ref -> 'a elt array -> char -> 'a elt -> unit
  = fun record tree children chr node ->
    if record.count < 48
    then ( let pos = ref 0 in
           while Array.unsafe_get children !pos != empty_elt do incr pos done
         ; record.keys.!{Char.code chr} <- Char.unsafe_chr !pos
         ; record.count <- record.count + 1
         ; Array.unsafe_set children !pos node )
    else ( let node256 = n256 record.prefix in
           copy_header ~src:record ~dst:node256 ;
           let children = Array.init 256 (fun i ->
               let k = Char.code (record.keys.!{i}) in
               if k <> 48 then Array.unsafe_get children k else empty_elt) in
           add_child_n256 node256 children chr node ;
           tree := Node { header= Header node256; children } )

let add_child_n16
  : n16 record -> 'a elt ref -> 'a elt array -> char -> 'a elt -> unit
  = fun record tree children chr node ->
    if record.count < 16
    then ( let mask = (1 lsl record.count) - 1 in
           let bit = ref 0 in
           let idx = ref 0 in
           for i = 0 to 15 do if chr < record.keys.!{i} then bit := !bit lor (1 lsl i) done ;
           bit := !bit land mask ;
           if !bit <> 0
           then ( idx := ctz !bit
                ; n16_shift record.keys !idx
                ; Array.blit children !idx children (!idx + 1) (record.count - !idx) )
           else idx := record.count ;
           record.keys.!{!idx} <- chr ;
           Array.unsafe_set children (!idx) node ;
           record.count <- record.count + 1 )
    else ( let node48 = n48 record.prefix in
           for i = 0 to record.count - 1 do node48.keys.!{Char.code record.keys.!{i}} <- Char.unsafe_chr i done ;
           copy_header ~src:record ~dst:node48 ;
           let children' = Array.make 48 empty_elt in
           Array.blit children 0 children' 0 16 ;
           let null = ref empty_elt in
           add_child_n48 node48 null children' chr node ;
           tree := Node { header= Header node48; children= children' } )

let rec iter_child_n4 keys idx max chr =
  if idx < max then ( if Char.code chr <= Char.code keys.!{idx}
                      then idx
                      else iter_child_n4 keys (succ idx) max chr )
  else max

let add_child_n4
  : n4 record -> 'a elt ref -> 'a elt array -> char -> 'a elt -> unit
  = fun record tree children chr node ->
    if record.count < 4
    then ( let idx = iter_child_n4 record.keys 0 record.count chr in
           n4_shift record.keys idx ;
           Array.blit children idx children (idx + 1) (record.count - idx) ;
           record.keys.!{idx} <- chr ;
           Array.unsafe_set children (idx) node ;
           record.count <- record.count + 1 )
    else ( let node16 = n16 record.prefix in
           let children' = Array.make 16 empty_elt in
           Array.blit children 0 children' 0 4 ;
           Bytes.unsafe_blit record.keys 0 node16.keys 0 4 ;
           copy_header ~src:record ~dst:node16 ;
           let null = ref empty_elt in
           add_child_n16 node16 null children' chr node ;
           tree := Node { header= Header node16; children= children'; } )

let not_found = (-1)

let find_child
  : 'a node -> char -> int
  = fun { header= Header record; _ } chr ->
    let res = ref not_found in
    let code = Char.code chr in

    ( match record.kind with
      | N4 ->
        let m = record.count in
        if m > 0 && Char.code record.keys.!{0} = code
        then res := 0
        else if m > 1 && Char.code record.keys.!{1} = code
        then res := 1
        else if m > 2 && Char.code record.keys.!{2} = code
        then res := 2
        else if m > 3 && Char.code record.keys.!{3} = code
        then res := 3
      | N16 ->
        (* TODO(dinosaure): can be replaced by SSE instr. *)
        let bit = ref 0 in
        for i = 0 to 15 do if record.keys.!{i} = chr then bit := !bit lor (1 lsl i) done ;
        let mask = (1 lsl record.count) - 1 in
        if !bit land mask <> 0 then res := ctz !bit
      | N48 ->
        let i = Char.code (record.keys.!{code}) in
        if i <> 48 then res := i
      | N256 -> res := code
      | NULL -> (()[@coverage off]) )
  ; !res
;;

let check_prefix ~prefix ~prefix_length ~off key len =
  let max = min (min prefix_length 10) (len - off) in
  let idx = ref 0 in
  while !idx < max && prefix.!{!idx} = key.![off + !idx]
  do incr idx done ; !idx

let rec minimum = function
  | Leaf leaf -> leaf
  | Node { header= Header { kind= N4; _ }; children; } ->
    minimum (Array.unsafe_get children 0)
  | Node { header= Header { kind= N16; _ }; children; } ->
    minimum (Array.unsafe_get children 0)
  | Node { header= Header { kind= N48; keys; _ }; children; } ->
    let idx = ref 0 in
    while keys.!{!idx} = '\048' do incr idx done ;
    idx := Char.code keys.!{!idx} ; minimum (Array.unsafe_get children !idx)
  | Node { header= Header { kind= N256; _ }; children; } ->
    let idx = ref 0 in
    while Array.unsafe_get children !idx == empty_elt do incr idx done ;
    minimum (Array.unsafe_get children !idx)
  | Node { header= Header { kind= NULL; _ }; _ } -> invalid_arg "empty tree"

let prefix_mismatch ({ header= Header header; _ } as node) ~off key len =
  let plen = header.prefix_length in
  let max = min (min plen 10) (len - off) in
  let idx = ref 0 in
  while !idx < max && header.prefix.!{!idx} = key.![off + !idx]
  do incr idx done ;

  if !idx = max && plen > 10
  then
    ( let leaf = minimum (Node node) in
      let max = (min (String.length leaf.key) len) - off in
      while !idx < max - 4
            && String.unsafe_get_uint32 leaf.key (off + !idx) = String.unsafe_get_uint32 key (off + !idx)
      do idx := !idx + 4 done ;
      while !idx < max
            && leaf.key.![off + !idx] = key.![off + !idx]
      do incr idx done ) ;
  !idx
;;

let longest_common_prefix ~off k1 k2 =
  let max = (min (String.length k1) (String.length k2)) - off in
  let idx = ref 0 in
  while !idx < max && k1.![off + !idx] = k2.![off + !idx]
  do incr idx done ; !idx

let leaf_matches { key; _ } ~off key' len' =
  if String.length key <> len' then raise Not_found ;
  if len' - off > 0 then memcmp key key' ~off ~len:(len' - off)
(* TODO(dinosaure): check all the key, (see optimistic match). *)

let rec _find ~key ~key_len depth = function
  | Leaf leaf ->
    leaf_matches leaf key ~off:depth key_len ; leaf.value
  | Node { header= Header { kind= NULL; _ }; _ } -> raise Not_found
  | Node ({ header= Header header; children; } as node) ->
    let plen = header.prefix_length in
    let depth =
      if plen <> 0
      then ( let plen' = check_prefix ~prefix:header.prefix ~prefix_length:plen ~off:depth key key_len in
             if plen' <> min 10 plen then raise Not_found
           ; depth + plen )
      else depth in
    let x = find_child node key.![depth] in
    if x = not_found || Array.unsafe_get children x == empty_elt
    then raise Not_found
    else _find ~key ~key_len (depth + 1) (Array.unsafe_get children x)

let find tree key =
  let key_len = String.length key in
  _find ~key ~key_len 0 !tree

let find_opt tree key =
  match find tree key with
  | v -> Some v
  | exception Not_found -> None

let rec insert tree elt key_a len_a value_a depth = match elt with
  | Node { header= Header { kind= NULL; _ }; _ } ->
    tree := (Leaf { key= key_a; value= value_a; })
  | Node ({ header= Header record; children; } as node) ->
    let plen = record.prefix_length in
    let pdiff = prefix_mismatch node ~off:depth key_a len_a in

    if pdiff >= plen
    then
      let chr = key_a.![depth + plen] in
      let leaf = Leaf { key= key_a; value= value_a; } in
      match find_child node chr, record.kind with
      | -1, N256 -> add_child_n256 record children chr leaf
      | -1, N48  -> add_child_n48  record tree children chr leaf
      | -1, N16  -> add_child_n16  record tree children chr leaf
      | -1, N4   -> add_child_n4   record tree children chr leaf
      | idx, _ ->
        let cur = ref (Array.unsafe_get children (idx)) in
        insert cur (Array.unsafe_get children idx) key_a len_a value_a (depth + plen + 1) ;
        Array.unsafe_set children idx !cur
    else
      ( let node4 = n4 () in
        let children' = Array.make 4 empty_elt in
        let null = ref empty_elt in
        node4.prefix_length <- pdiff
      ; Bytes.unsafe_blit record.prefix 0 node4.prefix 0 (min 10 pdiff)
      ; if plen <= 10
        then ( add_child_n4 node4 null children' record.prefix.!{pdiff} elt
             ; let plen' = plen - (pdiff + 1) in
               record.prefix_length <- plen'
             ; Bytes.unsafe_blit record.prefix (pdiff + 1) record.prefix 0 (min 10 plen') )
        else ( let plen' = plen - (pdiff + 1) in
               record.prefix_length <- plen'
             ; let bot = minimum elt in
               add_child_n4 node4 null children' bot.key.![depth + pdiff] elt
             ; Bytes.blit_string bot.key (depth + pdiff + 1) record.prefix 0 (min 10 plen') )
      ; add_child_n4 node4 null children' key_a.![depth + pdiff] (Leaf { key= key_a; value= value_a; })
      ; tree := (Node { header= Header node4; children= children'; }) )
  | Leaf leaf ->
    try
      leaf_matches leaf ~off:depth key_a len_a ; tree := (Leaf { leaf with value= value_a })
    with Not_found ->
      let node4 = n4 () in
      let children = Array.make 4 empty_elt in
      let null = ref empty_elt in
      let plon = longest_common_prefix ~off:depth leaf.key key_a in
      node4.prefix_length <- plon ;
      Bytes.blit_string key_a depth node4.prefix 0 (min 10 plon) ;
      add_child_n4 node4 null children leaf.key.![depth + plon] elt ;
      add_child_n4 node4 null children key_a.![depth + plon] (Leaf { key= key_a; value= value_a; }) ;
      tree := (Node { header= Header node4; children; })
;;

let insert tree key value =
  insert tree !tree key (String.length key) value 0

let minimum tree =
  let { value; key; } = minimum !tree in
  key, value

let make () = ref empty_elt

let remove_child_n256
  : n256 record -> 'a elt ref -> 'a elt array -> char -> unit
  = fun record tree children chr ->
    children.(Char.code chr) <- empty_elt ;
    record.count <- record.count - 1 ;
    if record.count = 37
    then ( let node48 = n48 record.prefix in
           copy_header ~src:record ~dst:node48 ;
           let children' = Array.make 48 empty_elt in
           let pos = ref 0 in
           for i = 0 to 255 do
             if children.(i) != empty_elt
             then ( children'.(!pos) <- children.(i)
                  ; node48.keys.!{i} <- Char.unsafe_chr !pos
                  ; incr pos )
           done ;
           tree := Node { header= Header node48; children= children' } )

let remove_child_n48
  : n48 record -> 'a elt ref -> 'a elt array -> char -> unit
  = fun record tree children chr ->
    let pos = Char.code record.keys.!{Char.code chr} in
    record.keys.!{Char.code chr} <- '\048' ;
    children.(pos) <- empty_elt ;
    record.count <- record.count - 1 ;
    if record.count = 12
    then ( let node16 = n16 record.prefix in
           let children' = Array.make 16 empty_elt in
           copy_header ~src:record ~dst:node16 ;
           let child = ref 0 in
           for i = 0 to 255 do
             let pos = Char.code record.keys.!{i} in
             if pos <> 48
             then ( node16.keys.!{!child} <- Char.chr i
                  ; children'.(!child) <- children.(pos)
                  ; incr child )
           done ;
           tree := Node { header= Header node16; children= children' } )

let remove_child_n16
  : n16 record -> 'a elt ref -> 'a elt array -> int -> unit
  = fun record tree children pos ->
    Bytes.blit record.keys (pos + 1) record.keys pos (record.count - 1 - pos) ;
    Array.blit children (pos + 1) children pos (record.count - 1 - pos) ;
    for pos = record.count - 1 to 15 do children.(pos) <- empty_elt done ;
    record.count <- record.count - 1 ;
    if record.count == 3
    then ( let node4 = n4 () in
           let children' = Array.make 4 empty_elt in
           Bytes.unsafe_blit record.keys 0 node4.keys 0 3
         ; Array.blit children 0 children' 0 3
         ; copy_header ~src:record ~dst:node4
         ; tree := Node { header= Header node4; children= children' } )

let unsafe_get_key : type a. a record -> int -> char = fun record n -> match record.kind with
  | N4 -> Bytes.unsafe_get record.keys n
  | N16 -> Bytes.unsafe_get record.keys n
  | N48 -> Bytes.unsafe_get record.keys n
  | N256 -> Char.unsafe_chr n
  | NULL -> (assert false[@coverage off])

let remove_child_n4
  : n4 record -> 'a elt ref -> 'a elt array -> int -> unit
  = fun record tree children pos ->
    Bytes.blit record.keys (pos + 1) record.keys pos (record.count - 1 - pos) ;
    Array.blit children (pos + 1) children pos (record.count - 1 - pos) ;
    for pos = record.count  - 1 to 3 do children.(pos) <- empty_elt done ;
    if (record.count - 1 - pos) = 0 then children.(pos) <- empty_elt ;
    (* XXX(dinosaure): remove trailing children. *)
    record.count <- record.count - 1 ;
    if record.count = 1
    then
      match children.(0) with
      | Leaf _ -> tree := children.(0)
      | Node { header= Header ({ prefix_length; _ } as hdr); _ } as child ->
        let prefix = ref record.prefix_length in
        if !prefix < 10
        then ( Bytes.unsafe_set record.prefix !prefix (unsafe_get_key record 0)
             ; incr prefix ) ;
        if !prefix < 10
        then ( let sub = min prefix_length (10 - !prefix) in
               Bytes.blit hdr.prefix 0 record.prefix !prefix sub ;
               prefix := !prefix + sub ) ;
        Bytes.blit record.prefix 0 hdr.prefix 0 (min !prefix 10) ;
        hdr.prefix_length <- hdr.prefix_length + record.prefix_length + 1 ;
        tree := child

let remove_child
  : 'a node -> 'a elt ref -> char -> int -> unit
  = fun { header= Header record; children } tree chr pos ->
    match record.kind with
    | N4 -> remove_child_n4 record tree children pos
    | N16 -> remove_child_n16 record tree children pos
    | N48 -> remove_child_n48 record tree children chr
    | N256 -> remove_child_n256 record tree children chr
    | NULL -> (()[@coverage off])

let rec remove
  : 'a elt -> 'a t -> string -> int -> int -> unit
  = fun elt tree key key_len depth -> match elt with
    | Node ({ header= Header header; children; } as node) ->
      let plen = header.prefix_length in
      let depth =
        if plen <> 0
        then ( let plen' = check_prefix ~prefix:header.prefix ~prefix_length:plen ~off:depth key key_len in
               if plen' <> min 10 plen then raise Not_found
             ; depth + plen )
        else depth in
      let x = find_child node key.![depth] in
      if x = not_found || Array.unsafe_get children x == empty_elt
      then raise Not_found
      else
        ( match children.(x) with
        | Leaf leaf ->
          leaf_matches leaf ~off:depth key key_len ;
          remove_child node tree key.![depth] x
        | Node _ as child ->
          let cur = ref child in
          remove child cur key key_len (succ depth)
        ; children.(x) <- !cur )
    | Leaf leaf ->
      leaf_matches leaf ~off:depth key key_len ; tree := empty_elt

let remove tree key =
  if !tree == empty_elt then raise Not_found ;
  remove !tree tree key (String.length key) 0

let rec iter ~f acc = function
  | Leaf { key; value; } -> f key value acc
  | Node { children; _ } ->
    let acc = ref acc in
    for i = 0 to Array.length children - 1 do acc := iter ~f !acc children.(i) done ;
    !acc
(* XXX(dinosaure): [empty_elt] has no children. *)

let iter ~f acc tree = iter ~f acc !tree

type 'a enumerate =  End | More of key * 'a * 'a elt * 'a enumerate

let rec cons_enum elt e = match elt with
  | Leaf { key; value; } -> More (key, value, empty_elt, e)
  | Node { children; _ } ->
    Array.fold_left (fun e elt -> cons_enum elt e) e children

let rec seq_of_enum c () = match c with
  | End -> Seq.Nil
  | More (k, v, t, r) -> Seq.Cons ((k, v), seq_of_enum (cons_enum t r))

let to_seq tree = seq_of_enum (cons_enum !tree End)

let of_seq seq =
  let tree = make () in
  Seq.iter (fun (k, v) -> insert tree k v) seq ;
  tree
