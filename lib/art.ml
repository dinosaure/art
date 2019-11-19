module String = struct
  include Stdlib.String

  external unsafe_get_uint32 : string -> int -> int32 = "%caml_string_get32"
end

let ( .![] ) = String.unsafe_get
let ( .!{} ) = Bytes.unsafe_get
let ( .!() ) = Array.unsafe_get
let ( .!()<- ) = Array.unsafe_set

type 'a node =
  { header : header
  ; children : 'a elt array }
and 'a leaf =
  { value : 'a
  ; key : string }
and 'a elt =
  | Leaf of 'a leaf
  | Node of 'a node
and header = Header : 'a record -> header
and 'a record =
  { prefix : bytes 
  ; mutable prefix_length : int
  ; mutable count : int
  ; kind : 'a kind
  ; keys : 'a }
and 'a kind =
  | N4   : n4   kind
  | N16  : n16  kind
  | N48  : n48  kind
  | N256 : n256 kind
  | NULL : unit kind
and n4 =
  { mutable n0_1 : int
  ; mutable n2_3 : int }
and n16 =
  { mutable n0  : int
  ; mutable n3  : int
  ; mutable n6  : int
  ; mutable n9  : int
  ; mutable n12 : int
  ; mutable n16 : int }
and n48 = int array
and n256 = int array

let pp_char ppf = function
  | '\x21' .. '\x7e' as chr -> Fmt.char ppf chr
  | chr -> Fmt.pf ppf "%02x" (Char.code chr)

let pp_n4 ppf { n0_1; n2_3; } =
  Fmt.pf ppf "%a" Fmt.(Dump.array (using Char.unsafe_chr pp_char)) [| n0_1 land 0xff; n0_1 asr 8; n2_3 land 0xff; n2_3 asr 8 |]

let pp_n16 ppf { n0; n3; n6; n9; n12; n16; } =
  let to_array n = [| n land 0xff; (n asr 8) land 0xff; (n asr 16) land 0xff |] in
  Fmt.pf ppf "%a"
    Fmt.(Dump.array (using Char.unsafe_chr pp_char))
    (Array.concat [ to_array n0; to_array n3; to_array n6; to_array n9; to_array n12; [| n16 |] ])

let pp_n48 = Fmt.(Dump.array int)
let pp_n256 = Fmt.(Dump.array int)

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

let pp pp_value ppf { contents } = pp_elt pp_value ppf contents

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
  | 3 -> () | _ -> invalid_arg "n4_shift"

let n4_set n4 idx chr = match idx with
  | 0 -> n4.n0_1 <- n4.n0_1 lor chr
  | 1 -> n4.n0_1 <- n4.n0_1 lor (chr lsl 8)
  | 2 -> n4.n2_3 <- n4.n2_3 lor chr
  | 3 -> n4.n2_3 <- n4.n2_3 lor (chr lsl 8)
  | _ -> invalid_arg "n4_set"

let n16 () : n16 record =
  let prefix = Bytes.make 10 '\000' in
  let record =
    { prefix; prefix_length= 0;
      count= 0;
      kind= N16; keys= { n0= 0; n3= 0; n6= 0; n9= 0; n12= 0; n16= 0; } } in
  record

let n16_iteri ~f n16 =
  f 0 (n16.n0 land 0xff)   ; f 1  ((n16.n0  asr 8) land 0xff) ; f 2  ((n16.n0  asr 16)  land 0xff) ;
  f 3 (n16.n3 land 0xff)   ; f 4  ((n16.n3  asr 8) land 0xff) ; f 5  ((n16.n3  asr 16)  land 0xff) ;
  f 6 (n16.n6 land 0xff)   ; f 7  ((n16.n6  asr 8) land 0xff) ; f 8  ((n16.n6  asr 16)  land 0xff) ;
  f 9 (n16.n9 land 0xff)   ; f 10 ((n16.n9  asr 8) land 0xff) ; f 11 ((n16.n9  asr 16)  land 0xff) ;
  f 12 (n16.n12 land 0xff) ; f 13 ((n16.n12 asr 8) land 0xff) ; f 14 ((n16.n12 asr 16) land 0xff) ;
  f 15 (n16.n16 land 0xff)
;;

let n16_set n16 idx chr = match idx with
  | 0  -> n16.n0  <- n16.n0  lor chr
  | 1  -> n16.n0  <- n16.n0  lor (chr lsl 8)
  | 2  -> n16.n0  <- n16.n0  lor (chr lsl 16)
  | 3  -> n16.n3  <- n16.n3  lor chr
  | 4  -> n16.n3  <- n16.n3  lor (chr lsl 8)
  | 5  -> n16.n3  <- n16.n3  lor (chr lsl 16)
  | 6  -> n16.n6  <- n16.n6  lor chr
  | 7  -> n16.n6  <- n16.n6  lor (chr lsl 8)
  | 8  -> n16.n6  <- n16.n6  lor (chr lsl 16)
  | 9  -> n16.n9  <- n16.n9  lor chr
  | 10 -> n16.n9  <- n16.n9  lor (chr lsl 8)
  | 11 -> n16.n9  <- n16.n9  lor (chr lsl 16)
  | 12 -> n16.n12 <- n16.n12 lor chr
  | 13 -> n16.n12 <- n16.n12 lor (chr lsl 8)
  | 14 -> n16.n12 <- n16.n12 lor (chr lsl 16)
  | 15 -> n16.n16 <- chr
  | _ -> invalid_arg "n16_set"

let n16_shift n16 = function
  | 0 ->
    n16.n16 <- n16.n12 asr 16
  ; n16.n12 <- (n16.n12 lsl 8) lor ((n16.n9 asr 16) land 0xff)
  ; n16.n9  <- (n16.n9  lsl 8) lor ((n16.n6 asr 16) land 0xff)
  ; n16.n6  <- (n16.n6  lsl 8) lor ((n16.n3 asr 16) land 0xff)
  ; n16.n3  <- (n16.n3  lsl 8) lor ((n16.n0 asr 16) land 0xff)
  ; n16.n0  <- (n16.n0  lsl 8)
  | 1 ->
    n16.n16 <- n16.n12 asr 16
  ; n16.n12 <- (n16.n12 lsl 8) lor ((n16.n9 asr 16) land 0xff)
  ; n16.n9  <- (n16.n9  lsl 8) lor ((n16.n6 asr 16) land 0xff)
  ; n16.n6  <- (n16.n6  lsl 8) lor ((n16.n3 asr 16) land 0xff)
  ; n16.n3  <- (n16.n3  lsl 8) lor ((n16.n0 asr 16) land 0xff)
  ; n16.n0  <- (n16.n0  lsl 8) lor (n16.n0 land 0xff)
  | 2 ->
    n16.n16 <- n16.n12 asr 16
  ; n16.n12 <- (n16.n12 lsl 8) lor ((n16.n9 asr 16) land 0xff)
  ; n16.n9  <- (n16.n9  lsl 8) lor ((n16.n6 asr 16) land 0xff)
  ; n16.n6  <- (n16.n6  lsl 8) lor ((n16.n3 asr 16) land 0xff)
  ; n16.n3  <- (n16.n3  lsl 8) lor ((n16.n0 asr 16) land 0xff)
  ; let n0 = n16.n0 in n16.n0 <- n0 land 0xffff

  | 3 ->
    n16.n16 <- n16.n12 asr 16
  ; n16.n12 <- (n16.n12 lsl 8) lor ((n16.n9 asr 16) land 0xff)
  ; n16.n9  <- (n16.n9  lsl 8) lor ((n16.n6 asr 16) land 0xff)
  ; n16.n6  <- (n16.n6  lsl 8) lor ((n16.n3 asr 16) land 0xff)
  ; n16.n3  <- (n16.n3  lsl 8)
  | 4 ->
    n16.n16 <- n16.n12 asr 16
  ; n16.n12 <- (n16.n12 lsl 8) lor ((n16.n9 asr 16) land 0xff)
  ; n16.n9  <- (n16.n9  lsl 8) lor ((n16.n6 asr 16) land 0xff)
  ; n16.n6  <- (n16.n6  lsl 8) lor ((n16.n3 asr 16) land 0xff)
  ; n16.n3  <- (n16.n3  lsl 8) lor (n16.n3 land 0xff)
  | 5 ->
    n16.n16 <- n16.n12 asr 16
  ; n16.n12 <- (n16.n12 lsl 8) lor ((n16.n9 asr 16) land 0xff)
  ; n16.n9  <- (n16.n9  lsl 8) lor ((n16.n6 asr 16) land 0xff)
  ; n16.n6  <- (n16.n6  lsl 8) lor ((n16.n3 asr 16) land 0xff)
  ; let n3 = n16.n3 in n16.n3 <- n3 land 0xffff

  | 6 ->
    n16.n16 <- n16.n12 asr 16
  ; n16.n12 <- (n16.n12 lsl 8) lor ((n16.n9 asr 16) land 0xff)
  ; n16.n9  <- (n16.n9  lsl 8) lor ((n16.n6 asr 16) land 0xff)
  ; n16.n6  <- (n16.n6  lsl 8)
  | 7 ->
    n16.n16 <- n16.n12 asr 16
  ; n16.n12 <- (n16.n12 lsl 8) lor ((n16.n9 asr 16) land 0xff)
  ; n16.n9  <- (n16.n9  lsl 8) lor ((n16.n6 asr 16) land 0xff)
  ; n16.n6  <- (n16.n6  lsl 8) lor (n16.n6 land 0xff)
  | 8 ->
    n16.n16 <- n16.n12 asr 16
  ; n16.n12 <- (n16.n12 lsl 8) lor ((n16.n9 asr 16) land 0xff)
  ; n16.n9  <- (n16.n9  lsl 8) lor ((n16.n6 asr 16) land 0xff)
  ; let n6 = n16.n6 in n16.n6 <- n6 land 0xffff

  | 9 ->
    n16.n16 <- n16.n12 asr 16
  ; n16.n12 <- (n16.n12 lsl 8) lor ((n16.n9 asr 16) land 0xff)
  ; n16.n9  <- (n16.n9  lsl 8)
  | 10 ->
    n16.n16 <- n16.n12 asr 16
  ; n16.n12 <- (n16.n12 lsl 8) lor ((n16.n9 asr 16) land 0xff)
  ; n16.n9  <- (n16.n9  lsl 8) lor (n16.n9 land 0xff)
  | 11 ->
    n16.n16 <- n16.n12 asr 16
  ; n16.n12 <- (n16.n12 lsl 8) lor ((n16.n9 asr 16) land 0xff)
  ; let n9 = n16.n9 in n16.n9 <- n9 land 0xffff

  | 12 ->
    n16.n16 <- n16.n12 asr 16
  ; n16.n12 <- (n16.n12 lsl 8)
  | 13 ->
    n16.n16 <- n16.n12 asr 16
  ; n16.n12 <- (n16.n12 lsl 8) lor (n16.n12 land 0xff)
  | 14 ->
    n16.n16 <- n16.n12 asr 16
  ; let n12 = n16.n12 in n16.n12 <- n12 land 0xffff

  | 15 -> () | _ -> invalid_arg "n16_shift"

let n48 () : n48 record =
  let prefix = Bytes.make 10 '\000' in
  let keys = Array.make 256 0 in
  let record =
    { prefix; prefix_length= 0;
      count= 0;
      kind= N48; keys; } in
  record

let n256 () : n256 record =
  let prefix = Bytes.make 10 '\000' in
  let keys = Array.make 256 0 in
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
    then raise Not_found ;
  done ;

  for i = 0 to len0 - 1 do
    let i = off + len1 * 4 + i in
    if a.![i] <> b.![i] then raise Not_found ;
  done
;;

let copy_header : type a b. src:a record -> dst:b record -> unit = fun ~src ~dst ->
  dst.count <- src.count ;
  Bytes.unsafe_blit src.prefix 0 dst.prefix 0 10 ;
  dst.prefix_length <- src.prefix_length

let add_child_n256
  : n256 record -> 'a elt array -> char -> 'a elt -> unit
  = fun record children chr node ->
    record.count <- record.count + 1 ;
    children.!(Char.code chr) <- node

let add_child_n48
  : n48 record -> (n256 record -> 'a elt array -> unit) -> 'a elt array -> char -> 'a elt -> unit
  = fun record kgrow children chr node ->
    if record.count < 48
    then ( let pos = ref 0 in
           while children.!(!pos) != empty_elt do incr pos done
         ; record.keys.!(Char.code chr) <- (!pos + 1)
         ; record.count <- record.count + 1
         ; children.!(!pos) <- node )
    else ( let node256 = n256 () in
           copy_header ~src:record ~dst:node256 ;
           let children = Array.init 256 (fun i ->
               let k = record.keys.!(i) in
               if k > 0 then children.!(k - 1) else empty_elt) in
           add_child_n256 node256 children chr node ; kgrow node256 children )

let ignore_n48 : n48 record -> 'a elt array -> unit = fun _ _ -> assert false

let add_child_n16
  : n16 record -> (n48 record -> 'a elt array -> unit) -> 'a elt array -> char -> 'a elt -> unit
  = fun record kgrow children chr node ->
    if record.count < 16
    then ( let mask = (1 lsl record.count) - 1 in
           let bit = ref 0 in
           let idx = ref 0 in
           n16_iteri ~f:(fun i v -> if Char.code chr = v then bit := !bit lor (1 lsl i)) record.keys ;
           bit := !bit land mask ;
           if !bit <> 0
           then ( idx := ctz !bit
                ; n16_shift record.keys !idx
                ; Array.blit children !idx children (!idx + 1) (record.count - !idx) )
           else idx := record.count ;
           n16_set record.keys !idx (Char.code chr) ;
           children.!(!idx) <- node ;
           record.count <- record.count + 1 )
    else ( let node48 = n48 () in
           let f i v = node48.keys.!(v) <- i + 1 in
           n16_iteri ~f record.keys ;
           copy_header ~src:record ~dst:node48 ;
           let children' = Array.make 48 empty_elt in
           Array.blit children 0 children' 0 16 ;
           add_child_n48 node48 ignore_n48 children' chr node ; kgrow node48 children' )

let add_child_n4
  : n4 record -> (n16 record -> 'a elt array -> unit) -> 'a elt array -> char -> 'a elt -> unit
  = fun record kgrow children chr node ->
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
           children.!(!idx) <- node ;
           record.count <- record.count + 1 )
    else ( let node16 = n16 () in
           let children' = Array.make 16 empty_elt in
           Array.blit children 0 children' 0 4 ;
           node16.keys.n0 <- record.keys.n0_1 lor ((record.keys.n2_3 land 0xff) lsl 16) ;
           node16.keys.n3 <- record.keys.n2_3 asr 8 ;
           copy_header ~src:record ~dst:node16 ;
           add_child_n16 node16 ignore_n48 children' chr node ; kgrow node16 children' )

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
        then res := 0 ;
        if m > 1 && (record.keys.n0_1 asr 8) land 0xff = code
        then res := 1 ;
        if m > 2 && record.keys.n2_3 land 0xff = code
        then res := 2 ;
        if m > 3 && (record.keys.n2_3 asr 8) land 0xff = code
        then res := 3
      | N16 ->
        let bit = ref 0 in
        let f i v = if v = code then bit := !bit lor (1 lsl i) in
        n16_iteri ~f record.keys ;
        let mask = (1 lsl record.count) - 1 in
        if !bit land mask <> 0 then res := ctz !bit
      | N48 ->
        let i = record.keys.!(code) in
        if i <> 0 then res := i - 1
      | N256 ->
        if record.keys.!(code) <> 0
        then res := code
      | NULL -> () )
  ; !res
;;

let check_prefix
  : type a. a record -> off:int -> string -> int -> int
  = fun { prefix; prefix_length; _ } ~off key len ->
  let max = min (min prefix_length 10) (len - off) in
  let idx = ref 0 in
  while !idx < max && prefix.!{!idx} = key.![off + !idx]
  do incr idx done ; !idx

let rec minimum = function
  | Leaf leaf -> leaf
  | Node { header= Header { kind= N4; _ }; children; } ->
    minimum (children.!(0))
  | Node { header= Header { kind= N16; _ }; children; } ->
    minimum (children.!(0))
  | Node { header= Header { kind= N48; keys; _ }; children; } ->
    let idx = ref 0 in
    while keys.!(!idx) = 0 do incr idx done ;
    idx := keys.!(!idx) - 1 ; minimum children.!(!idx)
  | Node { header= Header { kind= N256; _ }; children; } ->
    let idx = ref 0 in
    while children.!(!idx) != empty_elt do incr idx done ;
    minimum children.!(!idx)
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
      while !idx < max && leaf.key.![off + !idx] = key.![off + !idx] do incr idx done ) ;
  !idx
;;

let longest_common_prefix ~off k1 k2 =
  let max = (min (String.length k1) (String.length k2)) - off in
  let idx = ref 0 in
  while !idx < max && k1.![off + !idx] = k2.![off + !idx]
  do incr idx done ; !idx

let leaf_matches { key; _ } ~off key' len' =
  if String.length key <> len' then raise Not_found ;
  memcmp key key' ~off ~len:(len' - off)

let find tree key =
  let len = String.length key in
  let rec go depth = function
    | Leaf leaf ->
      leaf_matches leaf key ~off:depth len ; leaf.value
    | Node { header= Header { kind= NULL; _ }; _ } -> raise Not_found
    | Node ({ header= Header header; children; } as node) ->
      let plen = header.prefix_length in
      let depth =
        if plen <> 0
        then ( let plen' = check_prefix header ~off:depth key len in
               if plen' <> min 10 plen then raise Not_found
             ; depth + plen )
        else depth in
      let x = find_child node key.![depth] in
      if x = not_found
      then raise Not_found
      else go (depth + 1) children.!(x) in
  go 0 !tree

let find_opt tree key =
  match find tree key with
  | v -> Some v
  | exception Not_found -> None

let ignore_n16 : n16 record -> 'a elt array -> unit = fun _ _ -> assert false

let rec insert kset elt key_a len_a value_a depth = match elt with
  | Node { header= Header { kind= NULL; _ }; _ } ->
    kset (Leaf { key= key_a; value= value_a; })
  | Node ({ header= Header record; children; } as node) ->
    let plen = record.prefix_length in
    let pdiff = prefix_mismatch node ~off:depth key_a len_a in

    if pdiff >= plen
    then
      let chr = key_a.![depth + plen] in
      let leaf = Leaf { key= key_a; value= value_a; } in
      let kgrow record children = kset (Node { header= Header record; children; }) in
      match find_child node chr, record.kind with
      | -1, N256 -> add_child_n256 record children chr leaf
      | -1, N48  -> add_child_n48  record kgrow children chr leaf
      | -1, N16  -> add_child_n16  record kgrow children chr leaf
      | -1, N4   -> add_child_n4   record kgrow children chr leaf
      | idx, _ ->
        let kset elt = children.!(idx) <- elt in
        insert kset children.!(idx) key_a len_a value_a (depth + plen + 1)
    else
      ( let node4 = n4 () in
        let children' = Array.make 4 empty_elt in
        node4.prefix_length <- pdiff
      ; Bytes.unsafe_blit record.prefix 0 node4.prefix 0 (min 10 pdiff)
      ; if plen <= 10
        then ( add_child_n4 node4 ignore_n16 children' record.prefix.!{pdiff} elt
             ; let plen' = plen - (pdiff + 1) in
               record.prefix_length <- plen'
             ; Bytes.unsafe_blit record.prefix (pdiff + 1) record.prefix 0 (min 10 plen') )
        else ( let plen' = plen - (pdiff + 1) in
               record.prefix_length <- plen'
             ; let bot = minimum elt in
               add_child_n4 node4 ignore_n16 children' bot.key.![depth + pdiff] elt
             ; Bytes.blit_string bot.key (depth + pdiff + 1) record.prefix 0 (min 10 plen') )
      ; add_child_n4 node4 ignore_n16 children' key_a.![depth + pdiff] (Leaf { key= key_a; value= value_a; })
      ; kset (Node { header= Header node4; children= children'; }) )
  | Leaf leaf ->
    try leaf_matches leaf ~off:depth key_a len_a ; kset (Leaf { leaf with value= value_a })
    with Not_found ->
      let node4 = n4 () in
      let children = Array.make 4 empty_elt in
      let plon = longest_common_prefix ~off:depth leaf.key key_a in
      node4.prefix_length <- plon ;
      Bytes.blit_string key_a depth node4.prefix 0 (min 10 plon) ;
      add_child_n4 node4 ignore_n16 children leaf.key.![depth + plon] elt ;
      add_child_n4 node4 ignore_n16 children key_a.![depth + plon] (Leaf { key= key_a; value= value_a; }) ;
      kset (Node { header= Header node4; children; })
;;

let insert tree key value =
  let kset v = tree := v in
  insert kset !tree key (String.length key) value 0

type 'a t = 'a elt ref

let make = fun () -> { contents= empty_elt; }
