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
let ( .!() ) (arr : int array) i = Array.unsafe_get arr i
let ( .!()<- ) (arr : int array) i (v : int)= Array.unsafe_set arr i v

type key = string (* + \000 *)


let ( .![] ) str i = String.unsafe_get str i

(* XXX(dinosaure): [unsafe_get] is required for performance but should
   be safe. ART uses the '\000' at the end of a string to ensure:

   > keys must not be prefixes any other keys.

   According layout of an OCaml string into the runtime, we have a
   '\000' at the end (padding), at least one byte. If you are 
   paranoniac, you can replace this snippet by:

   > if v = String.length key then '\000' else String.get k v

   This safe way was fuzzed and we did not get any exception. *)

type 'a kind =
  | N4   : n4   kind
  | N16  : n16  kind
  | N48  : n48  kind
  | N256 : n256 kind
  | NULL : unit kind
and n4 =
  { mutable n0_1 : int
  ; mutable n2_3 : int }
and n16 = bytes
and n48 = int array
and n256 = bytes

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

type 'a t =
  { tree : 'a elt ref
  ; null : 'a elt ref }

let pp_char ppf = function
  | '\x21' .. '\x7e' as chr -> Fmt.char ppf chr
  | chr -> Fmt.pf ppf "%02x" (Char.code chr)

let pp_n4 ppf { n0_1; n2_3; } =
  Fmt.pf ppf "%a" Fmt.(Dump.array (using Char.unsafe_chr pp_char))
    [| n0_1 land 0xff; n0_1 asr 8; n2_3 land 0xff; n2_3 asr 8 |]

let pp_n16 ppf keys =
  Fmt.pf ppf "%a"
    Fmt.(Dump.array pp_char)
    (Array.init 16 (fun i -> keys.!{i}))

let pp_n48 = Fmt.(Dump.array int)

let pp_n256 ppf keys =
  Fmt.pf ppf "%a"
    Fmt.(Dump.array pp_char)
    (Array.init 256 (fun i -> keys.!{i}))

let pp_keys : type a. kind:a kind -> a Fmt.t = fun ~kind -> match kind with
  | N4 -> pp_n4
  | N16 -> pp_n16
  | N48 -> pp_n48
  | N256 -> pp_n256
  | NULL -> Fmt.nop

let pp_record : type a. a record Fmt.t = fun ppf r ->
  match r.kind with
  | NULL -> Fmt.string ppf "<null>"
  | _ ->
    Fmt.pf ppf "{ @[<hov>prefix= %S;@ \
                         prefix_length= %d;@ \
                         count= %d;@ \
                         value= @[<hov>%a@];@] }"
      (Bytes.unsafe_to_string r.prefix) r.prefix_length
      r.count
      (pp_keys ~kind:r.kind) r.keys

let pp_header ppf (Header record) = pp_record ppf record

let rec pp_elt pp_value ppf = function
  | Leaf { key; value; } ->
    Fmt.pf ppf "{:leaf @[<hov>key= %S;@ value= @[<hov>%a@];@] }" key pp_value value
  | Node { header= Header { kind= NULL; _ }; _ } ->
    Fmt.string ppf "<null>"
  | Node { header; children; } ->
    Fmt.pf ppf "{:node @[<hov>hdr= @[<hov>%a@];@ children= @[<hov>%a@];@] }"
      pp_header header Fmt.(Dump.array (pp_elt pp_value)) children

let pp pp_value ppf { tree; _ } = pp_elt pp_value ppf !tree

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
      kind= N4; keys= { n0_1= 0; n2_3= 0; } } in
  record

let n4_shift n4 = function
  | 0 ->
    n4.n2_3 <- ((n4.n2_3 lsl 8) land 0xffff) lor ((n4.n0_1 lsr 8) land 0xff)
  ; n4.n0_1 <- (n4.n0_1 lsl 8) land 0xffff
  | 1 ->
    n4.n2_3 <- ((n4.n2_3 lsl 8) land 0xffff) lor ((n4.n0_1 lsr 8) land 0xff)
  ; n4.n0_1 <- n4.n0_1 land 0xff
  | 2 ->
    n4.n2_3 <- (n4.n2_3 lsl 8) land 0xffff
  | _ -> ()

let n4_set n4 idx chr = match idx with
  | 0 -> n4.n0_1 <- n4.n0_1 lor chr
  | 1 -> n4.n0_1 <- n4.n0_1 lor (chr lsl 8)
  | 2 -> n4.n2_3 <- n4.n2_3 lor chr
  | 3 -> n4.n2_3 <- n4.n2_3 lor (chr lsl 8)
  | _ -> ()

let n16 prefix : n16 record =
  let record =
    { prefix; prefix_length= 0;
      count= 0;
      kind= N16; keys= Bytes.make 16 '\000' } in
  record

let n16_shift keys n =
  Bytes.unsafe_blit keys n keys (n + 1) (16 - (n + 1))

let n48 prefix : n48 record =
  let keys = Array.make 256 0 in (* alloc minor *)
  let record =
    { prefix; prefix_length= 0;
      count= 0;
      kind= N48; keys; } in
  record

let n256 prefix : n256 record =
  let keys = Bytes.make 256 '\000' in (* alloc minor *)
  let record =
    { prefix; prefix_length= 0;
      count= 0;
      kind= N256; keys; } in
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
  : n48 record -> 'a elt ref -> null:'a elt ref -> 'a elt array -> char -> 'a elt -> unit
  = fun record tree ~null:_ children chr node ->
    if record.count < 48
    then ( let pos = ref 0 in
           while Array.unsafe_get children !pos != empty_elt do incr pos done
         ; record.keys.!(Char.code chr) <- (!pos + 1)
         ; record.count <- record.count + 1
         ; Array.unsafe_set children !pos node )
    else ( let node256 = n256 record.prefix in
           copy_header ~src:record ~dst:node256 ;
           let children = Array.init 256 (fun i ->
               let k = record.keys.!(i) in
               if k > 0 then Array.unsafe_get children (k - 1) else empty_elt) in
           add_child_n256 node256 children chr node ;
           tree := Node { header= Header node256; children } )

let add_child_n16
  : n16 record -> 'a elt ref -> null:'a elt ref -> 'a elt array -> char -> 'a elt -> unit
  = fun record tree ~null children chr node ->
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
           for i = 0 to record.count - 1 do node48.keys.!(Char.code record.keys.!{i}) <- (i + 1) done ;
           copy_header ~src:record ~dst:node48 ;
           let children' = Array.make 48 empty_elt in
           Array.blit children 0 children' 0 16 ;
           add_child_n48 node48 null ~null children' chr node ;
           tree := Node { header= Header node48; children= children' } )

let add_child_n4
  : n4 record -> 'a elt ref -> null:'a elt ref -> 'a elt array -> char -> 'a elt -> unit
  = fun record tree ~null children chr node ->
    if record.count < 4
    then ( let idx = ref 0 in
           let max = record.count in
           let cmd = Char.code chr in
           if !idx < max && cmd >= record.keys.n0_1 land 0xff then incr idx ;
           if !idx < max && cmd >= record.keys.n0_1 asr 8 then incr idx ;
           if !idx < max && cmd >= record.keys.n2_3 land 0xff then incr idx ;
           if !idx < max && cmd >= record.keys.n2_3 asr 8 then incr idx ;
           n4_shift record.keys !idx ;
           Array.blit children !idx children (!idx + 1) (record.count - !idx) ;
           n4_set record.keys !idx (Char.code chr) ;
           Array.unsafe_set children (!idx) node ;
           record.count <- record.count + 1 )
    else ( let node16 = n16 record.prefix in
           let children' = Array.make 16 empty_elt in
           Array.blit children 0 children' 0 4 ;
           node16.keys.!{0} <- Char.unsafe_chr (record.keys.n0_1 land 0xff) ;
           node16.keys.!{1} <- Char.unsafe_chr (record.keys.n0_1 asr 8) ;
           node16.keys.!{2} <- Char.unsafe_chr (record.keys.n2_3 land 0xff) ;
           node16.keys.!{3} <- Char.unsafe_chr (record.keys.n2_3 asr 8) ;
           copy_header ~src:record ~dst:node16 ;
           add_child_n16 node16 null ~null children' chr node ;
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
        if m > 0 && record.keys.n0_1 land 0xff = code
        then res := 0
        else if m > 1 && (record.keys.n0_1 asr 8) land 0xff = code
        then res := 1
        else if m > 2 && record.keys.n2_3 land 0xff = code
        then res := 2
        else if m > 3 && (record.keys.n2_3 asr 8) land 0xff = code
        then res := 3
      | N16 ->
        let bit = ref 0 in
        for i = 0 to 15 do if record.keys.!{i} = chr then bit := !bit lor (1 lsl i) done ;
        let mask = (1 lsl record.count) - 1 in
        if !bit land mask <> 0 then res := ctz !bit
      | N48 ->
        let i = record.keys.!(code) in
        if i <> 0 then res := i - 1
      | N256 ->
        if Char.code record.keys.!{code} <> 0
        then res := code
      | NULL -> () )
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
    while keys.!(!idx) = 0 do incr idx done ;
    idx := keys.!(!idx) - 1 ; minimum (Array.unsafe_get children !idx)
  | Node { header= Header { kind= N256; _ }; children; } ->
    let idx = ref 0 in
    while Array.unsafe_get children !idx != empty_elt do incr idx done ;
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

let find tree key =
  let key_len = String.length key in
  (* alloc *) let rec go depth = function
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
      if x = not_found
      then raise Not_found
      else go (depth + 1) (Array.unsafe_get children x) in
  go 0 !(tree.tree)

let find_opt tree key =
  match find tree key with
  | v -> Some v
  | exception Not_found -> None

let rec insert ({ tree; null; } as v) elt key_a len_a value_a depth = match elt with
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
      | -1, N48  -> add_child_n48  record tree ~null children chr leaf
      | -1, N16  -> add_child_n16  record tree ~null children chr leaf
      | -1, N4   -> add_child_n4   record tree ~null children chr leaf
      | idx, _ ->
        let v = { v with tree= ref (Array.unsafe_get children (idx)) } in
        insert v (Array.unsafe_get children idx) key_a len_a value_a (depth + plen + 1) ;
        Array.unsafe_set children idx !(v.tree)
    else
      ( let node4 = n4 () in
        let children' = Array.make 4 empty_elt in
        node4.prefix_length <- pdiff
      ; Bytes.unsafe_blit record.prefix 0 node4.prefix 0 (min 10 pdiff)
      ; if plen <= 10
        then ( add_child_n4 node4 null ~null children' record.prefix.!{pdiff} elt
             ; let plen' = plen - (pdiff + 1) in
               record.prefix_length <- plen'
             ; Bytes.unsafe_blit record.prefix (pdiff + 1) record.prefix 0 (min 10 plen') )
        else ( let plen' = plen - (pdiff + 1) in
               record.prefix_length <- plen'
             ; let bot = minimum elt in
               add_child_n4 node4 null ~null children' bot.key.![depth + pdiff] elt
             ; Bytes.blit_string bot.key (depth + pdiff + 1) record.prefix 0 (min 10 plen') )
      ; add_child_n4 node4 null ~null children' key_a.![depth + pdiff] (Leaf { key= key_a; value= value_a; })
      ; tree := (Node { header= Header node4; children= children'; }) )
  | Leaf leaf ->
    try
      leaf_matches leaf ~off:depth key_a len_a ; tree := (Leaf { leaf with value= value_a })
    with Not_found ->
      let node4 = n4 () in
      let children = Array.make 4 empty_elt in
      let plon = longest_common_prefix ~off:depth leaf.key key_a in
      node4.prefix_length <- plon ;
      Bytes.blit_string key_a depth node4.prefix 0 (min 10 plon) ;
      add_child_n4 node4 null ~null children leaf.key.![depth + plon] elt ;
      add_child_n4 node4 null ~null children key_a.![depth + plon] (Leaf { key= key_a; value= value_a; }) ;
      tree := (Node { header= Header node4; children; })
;;

let insert tree key value =
  insert tree !(tree.tree) key (String.length key) value 0

let make () =
  { tree= ref empty_elt
  ; null= ref empty_elt }
