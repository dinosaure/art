let () = Printexc.record_backtrace true

let (.![]) = String.unsafe_get

let src = Logs.Src.create "rowex"
module Log = (val Logs.src_log src : Logs.LOG)

module String = struct
  include Stdlib.String

  external unsafe_get_uint32 : string -> int -> int32 = "%caml_string_get32"
end

external bytes_set16 : bytes -> int -> int -> unit = "%caml_bytes_set16u"
external bytes_set32 : bytes -> int -> int32 -> unit = "%caml_bytes_set32u"
external bytes_set64 : bytes -> int -> int64 -> unit = "%caml_bytes_set64u"
external string_get16 : string -> int -> int = "%caml_string_get16u"

let const x _ = x

let size_of_word = Sys.word_size / 8

external bswap64 : int64 -> int64 = "%bswap_int64"
external bswap32 : int32 -> int32 = "%bswap_int32"

(* XXX(dinosaure): [Int64.of_int]/[Int32.of_int] interprets the bit-sign, so
   [Addr.null = 0x4000000000000000] becomes [0xc000000000000000] when we want to
   serialize it. [uint64_of_uint]/[uint32_of_uint] uses [Unsigned_long_val] but
   it's a shame to add these C functions. The compiler should add such
   primitives. *)

external uint64_of_uint : int -> (int64[@unboxed]) =
  "bytecode_compilation_not_supported"
  "caml_uint64_of_uint" [@@noalloc]

external uint32_of_uint : int -> (int32[@unboxed]) =
  "bytecode_compilation_not_supported"
  "caml_uint32_of_uint" [@@noalloc]

let beintnat_to_string v =
  if Sys.word_size = 64 && Sys.big_endian
  then
    let v = bswap64 (uint64_of_uint v) in
    let res = Bytes.create 8 in
    bytes_set64 res 0 v ;
    Bytes.unsafe_to_string res
  else if Sys.word_size = 64
  then
    let v = uint64_of_uint v in
    let res = Bytes.create 8 in
    bytes_set64 res 0 v ;
    Bytes.unsafe_to_string res
  else if Sys.word_size = 32 && Sys.big_endian
  then
    let v = bswap32 (uint32_of_uint v) in
    let res = Bytes.create 4 in
    bytes_set32 res 0 v ;
    Bytes.unsafe_to_string res
  else if Sys.word_size = 32
  then
    let v = uint32_of_uint v in
    let res = Bytes.create 4 in
    bytes_set32 res 0 v ;
    Bytes.unsafe_to_string res
  else assert false (* TODO? *)
[@@inline]

let beint31_to_string v =
  if Sys.big_endian
  then
    let res = Bytes.create 4 in
    bytes_set32 res 0 (bswap32 (Int32.of_int v)) ;
    Bytes.unsafe_to_string res
  else
    let res = Bytes.create 4 in
    bytes_set32 res 0 (Int32.of_int v) ;
    Bytes.unsafe_to_string res
[@@inline]

(* XXX(dinosaure): same as the GC bit for OCaml, a native integer with the least
   significant bit to 1 is a leaf, otherwise, it is a node. Such layout is possible
   when **any** addresses are an even absolute number! To ensure that, the only
   value which can introduce an odd shift is a C string (something which terminates by
   a '\000') but we ensure that a C string has a "pad" of '\000' such as OCaml does.

   We use an native [int] here which depends on the architecture of the host
   system. We have multiple reason to do this choice:
   + Use an [int64] implies to use a boxed value which has an impact on
     performance (due to the indirection)
   + Use an [int64] implies a real performance regression on a 32-bits
     architecture (due to the impossibility to store it into a register)
   + A incompatibility exists if we move from a 32-bits to a 64-bits
     architecture - but it seems more clever to use a static tool which can
     upgrade (or downgrade) the format from one to another
   + A limitation exists about how many objects we can store - however, it seems
     fair to keep this limitation at our layer when it is applied at the whole
     system

   According to the layout, we are able to encode/{i serialise} an address into
   [Sys.word_size - 2] bits (GC bit plus ART-type bit). Then, the [NULL] address
   can be encoded to an impossible address (for us but it still is value for
   OCaml): [1 lsl (Sys.word_size - 2)]. This is our way to encode/{i serialise}
   this type:

   {[
      type 'a tree =
        | Leaf of 'a
        | Node of 'a t
      and 'a t = 'a tree option
   ]} *)

module rec Leaf : sig
  type t [@@immediate]

  val prj : t -> 'a Addr.t
  val inj : 'a Addr.t -> t
end = struct
  type t = int

  let prj x = x asr 1
  let inj x = (x lsl 1) lor 1
end

and Addr : sig
  type -'a t = private int constraint 'a = [< `Rd | `Wr ]

  val length : int

  val null : [ `Rd ] t
  val is_null : 'a t -> bool

  val of_int_rdonly : int -> [ `Rd ] t
  val of_int_wronly : int -> [ `Wr ] t
  val of_int_rdwr : int -> [ `Rd | `Wr ] t

  val to_wronly : [> `Wr ] t -> [ `Wr ] t
  val to_rdonly : [> `Rd ] t -> [ `Rd ] t

  val unsafe_to_leaf : 'a t -> Leaf.t
  val unsafe_of_leaf : Leaf.t -> 'a t

  val ( + ) : 'a t -> int -> 'a t
end = struct
  type -'a t = int constraint 'a = [< `Rd | `Wr ]

  let length = Sys.word_size / 8

  let null = 1 lsl (Sys.word_size - 2)
  let is_null x = x = null

  let of_int_rdonly x = (* assert (x land null = 0) *) x
  let of_int_wronly x = (* assert (x land null = 0) *) x
  let of_int_rdwr x = x
  let to_wronly x = x
  let to_rdonly x = x

  let unsafe_to_leaf x = x
  let unsafe_of_leaf x = x

  let ( + ) addr v = addr + v
end

let string_of_null_addr = beintnat_to_string (Addr.null :> int)

type ('c, 'a) value =
  | Int8     : ([ `Atomic ], int) value
  | BEInt    : ([ `Atomic ], int) value
  | BEInt16  : ([ `Atomic ], int) value
  | BEInt31  : ([ `Atomic ], int) value
  | BEInt64  : ([ `Atomic ], int64) value
  | BEInt128 : ([ `Atomic ], string) value
  (* XXX(dinosaure): a Int128 does not exist in OCaml, so we load it into a
       simple (big-endian) [string]. However, the access to the value must
       be atomic and be saved into a string then. *)
  | Addr_rd  : ([ `Atomic ], [ `Rd ] Addr.t) value
  | C_string : ([ `Non_atomic ], string) value

type 'c memory_order =
  | Relaxed : [< `Rd | `Wr ] memory_order
  | Seq_cst : [< `Rd | `Wr ] memory_order
  | Release : [< `Wr ] memory_order

type 'a t =
  | Atomic_get : [< `Rd ] memory_order * [> `Rd ] Addr.t * ([ `Atomic ], 'a) value -> 'a t
  | Atomic_set : [< `Wr ] memory_order * [> `Wr ] Addr.t * ([ `Atomic ], 'a) value * 'a -> unit t
  | Fetch_add  : [< `Rd | `Wr ] memory_order * [> `Rd | `Wr ] Addr.t * ([ `Atomic ], int) value * int -> unit t
  | Pause_intrinsic : unit t
  | Compare_exchange : [> `Rd | `Wr ] Addr.t *
                       ([ `Atomic ], 'a) value * 'a * 'a * bool * [< `Rd | `Wr ] memory_order -> bool t
  | Get : [> `Rd ] Addr.t * ('c, 'a) value -> 'a t
  | Allocate : string list * int -> [ `Rd | `Wr ] Addr.t t
  | Delete : _ Addr.t * int -> unit t
  | Collect : _ Addr.t * int -> unit t
  | Bind : 'a t * ('a -> 'b t) -> 'b t
  | Return : 'a -> 'a t

type 'a fmt = Format.formatter -> 'a -> unit

let pf ppf fmt = Format.fprintf ppf fmt

let pp_memory_order : type a. a memory_order fmt = fun ppf -> function
  | Seq_cst -> pf ppf "seq_cst"
  | Release -> pf ppf "release"
  | Relaxed -> pf ppf "relaxed"

let pp_value : type c a. (c, a) value fmt = fun ppf -> function
  | BEInt -> pf ppf "beintnat"
  | BEInt31 -> pf ppf "beint31"
  | BEInt16 -> pf ppf "beint16"
  | BEInt64 -> pf ppf "beint64"
  | BEInt128 -> pf ppf "beint128"
  | Int8 -> pf ppf "int8"
  | Addr_rd -> pf ppf "addr"
  | C_string -> pf ppf "c_string"

let fmt fmt = fun ppf -> pf ppf fmt

let pp_of_value : type c a. (c, a) value -> a fmt = function
  | BEInt -> fun ppf v -> if v < 0  then pf ppf "%16x" v else pf ppf "%10d" v
  | BEInt31 -> fmt "%10d"
  | BEInt16 -> fmt "%5d"
  | BEInt64 -> fmt "%19Ld"
  | BEInt128 -> fmt "%S"
  | Int8 -> fmt "%3d"
  | Addr_rd -> fun ppf addr -> pf ppf "%016x" (addr :> int)
  | C_string -> fmt "%S"

let pp : type a. a t fmt = fun ppf -> function
  | Atomic_get (m, addr, v) ->
     pf ppf "atomic_get %016x %a : %a" (addr :> int) pp_memory_order m pp_value v
  | Atomic_set (m, addr, v, x) ->
     pf ppf "atomic_set %016x %a (%a : %a)" (addr :> int) pp_memory_order m (pp_of_value v) x pp_value v
  | Fetch_add (m, addr, v, x) ->
     pf ppf "fetch_add  %016x %a (%a : %a)" (addr :> int) pp_memory_order m (pp_of_value v) x pp_value v
  | Collect (addr, len) ->
     pf ppf "collect    %016x %d" (addr :> int) len
  | Delete (addr, len) ->
     pf ppf "delete     %016x %d" (addr :> int) len
  | Get (addr, v) ->
     pf ppf "get        %016x         : %a" (addr :> int) pp_value v
  | Allocate (_, len) ->
     pf ppf "allocate %d" len
  | Pause_intrinsic ->
     pf ppf "pause_intrinsic"
  | Compare_exchange (addr, v, x, y, weak, m) ->
     pf ppf "compare_exchange weak:%b %016x %a (%a : %a) (%a : %a)" weak (addr :> int) pp_memory_order m
       (pp_of_value v) x pp_value v
       (pp_of_value v) y pp_value v
  | Bind (Allocate (_, len), _) ->
     pf ppf "allocate %d byte(s) >>= fun _ ->" len
  | Bind _ -> pf ppf ">>="
  | Return _ -> pf ppf "return *"

module Value = struct
  let int8 = Int8
  let beint16 = BEInt16
  let beint31 = BEInt31
  let beintnat = BEInt
  let beint64 = BEInt64
  let beint128 = BEInt128
  let addr_rd = Addr_rd
  let c_string = C_string
end

let ( let* ) x f = Bind (x, f)

let return x = Return x

let get addr value = Get (addr, value)

let atomic_get ?(memory_order= Seq_cst) addr k = Atomic_get (memory_order, addr, k)

let atomic_set ?(memory_order= Seq_cst) addr k v = Atomic_set (memory_order, addr, k, v)

let fetch_add  ?(memory_order= Seq_cst) addr k n = Fetch_add (memory_order, addr, k, n)

let compare_exchange ?(memory_order= Seq_cst) ?(weak= false) addr k expected desired =
  Compare_exchange (addr, k, expected, desired, weak, memory_order)

let pause_intrinsic = Pause_intrinsic

let ( <.> ) f g = fun x -> f (g x)

let allocate ?len payloads =
  let len = match len with
    | Some len -> len
    | None -> List.fold_right (( + ) <.> String.length) payloads 0 in
  Allocate (payloads, len)

let delete addr len = Delete (addr, len)

let collect addr len = Collect (addr, len)

(* XXX(dinosaure): impossible by the type-system. *)
(* let _ = atomic_get ~memory_order:Release (Addr.of_int_rdonly 0) Value.int8
   [Release] is only used to [store]/[set]. *)
(* let _ = atomic_set (Addr.of_int_rdonly 0) Value.int8 0
   Impossible to set a read-only value. *)
(* let _ = atomic_get (Addr.of_int_wronly 0) Value.int8
   Impossible to get a write-only value. *)
(* let _ = atomic_get (Addr.of_int_rdonly 0) Value.(string 10)
   Impossible to load atomically a non-atomic value *)

let _prefix = 4
let _header_prefix = 0
let _header_prefix_count = _header_prefix + _prefix
(* XXX(dinosaure): [prefix_count] is **not** the length of [prefix]
   if [prefix_count > _prefix]. In that case, we do a compression-path
   of [prefix_count] bytes and save only 4 bytes of this prefix into
   [prefix] to help a pessimistic check. *)

let _header_kind = _header_prefix_count + 4
let _header_depth = if Sys.word_size = 64 then _header_kind + 8 else _header_kind + 4
(* XXX(dinosaure): [depth] includes [prefix]. For example, we have a path-compression of
   4-bytes into our **second**-level node (and parent of it has no prefix):

   depth = parent(depth) + length(prefix) + 1 = 5

   [depth] is a **constant**, it permits to set the prefix safely while reading.
   [find] trusts on this value first before to look into [prefix]/[prefix_count].
   /!\ [length(prefix) != prefix_count] - see [prefix_count].
       [length(prefix) == min _prefix prefix_count].
*)

let _header_count = _header_depth + 4
let _header_compact_count = _header_count + 2

let _header_length = _header_compact_count + 2

let _bits_kind = Sys.word_size - 3

let _n4_kind   = 0b00
let _n16_kind  = 0b01
let _n48_kind  = 0b10
let _n256_kind = 0b11

let get_version addr = atomic_get Addr.(addr + _header_kind) Value.beintnat

let get_type addr =
  let* value = atomic_get ~memory_order:Relaxed Addr.(addr + _header_kind) Value.beintnat in
  return (value lsr _bits_kind)
[@@inline]

let get_prefix (addr : [> `Rd ] Addr.t) =
  let* value = atomic_get Addr.(addr + _header_prefix) Value.beint64 in
  let p0 = Int64.(to_int (logand value 0xffffL)) in
  let p1 = Int64.(to_int (logand (shift_right value 16) 0xffffL)) in
  let prefix = Bytes.create _prefix in
  bytes_set16 prefix 0 p0 ;
  bytes_set16 prefix 2 p1 ;
  return (Bytes.unsafe_to_string prefix, Int64.(to_int (shift_right value 32)))

let set_prefix addr ~prefix ~prefix_count =
  if prefix_count = 0
  then atomic_set ~memory_order:Release Addr.(addr + _header_prefix) Value.beintnat 0
  else
    let p0 = string_get16 prefix 0 in
    let p1 = string_get16 prefix 2 in
    let prefix = Int64.(logor (shift_left (of_int p0) 48) (shift_left (of_int p1) 32)) in
    let rs = Int64.(logor prefix (of_int prefix_count)) in
    atomic_set ~memory_order:Release Addr.(addr + _header_prefix) Value.beint64 rs

(**** FIND CHILD ****)

let n4_find_child addr k =
  let* _0 = atomic_get Addr.(addr + _header_length + 0) Value.int8 in
  if _0 = k then atomic_get Addr.(addr + _header_length + 4 + (Addr.length * 0)) Value.addr_rd else
  let* _1 = atomic_get Addr.(addr + _header_length + 1) Value.int8 in
  if _1 = k then atomic_get Addr.(addr + _header_length + 4 + (Addr.length * 1)) Value.addr_rd else
  let* _2 = atomic_get Addr.(addr + _header_length + 2) Value.int8 in
  if _2 = k then atomic_get Addr.(addr + _header_length + 4 + (Addr.length * 2)) Value.addr_rd else
  let* _3 = atomic_get Addr.(addr + _header_length + 3) Value.int8 in
  if _3 = k then atomic_get Addr.(addr + _header_length + 4 + (Addr.length * 3)) Value.addr_rd else
    ( Log.debug (fun m -> m "No child for %02x into N4" k)
    ; return Addr.null )

external n16_get_child : int -> int -> string -> int = "caml_n16_get_child" [@@noalloc]
external ctz : int -> int = "caml_ctz" [@@noalloc]

(* XXX(dinosaure): despite [art.ml], [N4] and [N16] aren't order.
   We must check all children. *)

let rec _n16_find_child addr k bitfield =
  if bitfield = 0 then return Addr.null
  else
    let p = ctz bitfield in
    let* k' = atomic_get Addr.(addr + _header_length + p) Value.int8 in
    let* value = atomic_get Addr.(addr + _header_length + 16 + (p * Addr.length)) Value.addr_rd in
    if not (Addr.is_null value) && k' = (k lxor 128)
    then return value
    else _n16_find_child addr k (bitfield lxor (1 lsl p))

let n16_find_child addr k =
  let* keys = atomic_get Addr.(addr + _header_length) Value.beint128 in
  (* XXX(dinosaure): Dragoon here! How to load atomically a 128 bits integer and save it into a string? *)
  let bitfield = n16_get_child 16 k keys in
  _n16_find_child addr k bitfield

let n48_find_child addr k =
  let* pos' = atomic_get Addr.(addr + _header_length + k) Value.int8 in
  if pos' <> 48 then atomic_get Addr.(addr + _header_length + 256 + (Addr.length * pos')) Value.addr_rd
  else return Addr.null

let n256_find_child addr k =
  atomic_get Addr.(addr + _header_length + (Addr.length * k)) Value.addr_rd

(**** MINIMUM ****)

(* XXX(dinosaure): we assume that the given node has, at least, one child.
   We prioritise /leaf/ - however, nothing assert that a node has, at least,
   one /leaf/...

   I hope that OCaml is able to inline and eta-expand [header] & [max]. *)

let rec _node_any_child addr ~header child idx max =
  if idx = max then return child
  else
    let* child' : [ `Rd ] Addr.t = atomic_get Addr.(addr + _header_length + header + (idx * Addr.length)) Value.addr_rd in
    if (child' :> int) land 1 = 1 then return child'
    else _node_any_child addr ~header (if not (Addr.is_null child') then child' else child) (succ idx) max
[@@inline]

let n4_any_child addr   = _node_any_child addr ~header:4   Addr.null 0 4
let n16_any_child addr  = _node_any_child addr ~header:16  Addr.null 0 16
let n48_any_child addr  = _node_any_child addr ~header:256 Addr.null 0 48
let n256_any_child addr = _node_any_child addr ~header:0   Addr.null 0 256

let any_child (addr : [> `Rd] Addr.t) =
  let* ty = get_type addr in
  match ty with
  | 0 -> n4_any_child addr
  | 1 -> n16_any_child addr
  | 2 -> n48_any_child addr
  | 3 -> n256_any_child addr
  | _ -> assert false

let rec minimum
  : [ `Rd ] Addr.t -> [ `Rd ] Addr.t t
  = fun addr ->
  if (addr :> int) land 1 = 1 then return addr
  else let* addr = any_child addr in minimum addr

let minimum (addr : [> `Rd ] Addr.t) = minimum (Addr.to_rdonly addr) [@@inline]

let find_child addr chr =
  let k = Char.code chr in
  let* ty = get_type addr in
  Log.debug (fun m -> m "find_child node %c" chr) ;
  match ty with
  | 0 -> n4_find_child   addr k
  | 1 ->
     Log.debug (fun m -> m "find_child_n16 node %c" chr) ;
     n16_find_child  addr k
  | 2 ->
     Log.debug (fun m -> m "find_child_n48 node %c" chr) ;
     n48_find_child  addr k
  | 3 -> n256_find_child addr k
  | _ -> assert false

let rec _check_prefix ~key ~key_len ~prefix ~level idx max =
  if idx < max
  then ( if prefix.[idx] <> key.[level + idx]
         then raise Not_found
         else _check_prefix ~key ~key_len ~prefix ~level (succ idx) max )
;;

(* XXX(dinosaure):

   [<=0]: match
   [>0]: optimistic match

   Optimistic match **skips** a certain number of bytes
   without comparing them! Optimistic match has a side-effect,
   it returns the new value of the level.
*)

let check_prefix addr ~key ~key_len level =
  let* depth = get Addr.(addr + _header_depth) Value.beint31 in
  if key_len < depth
  then raise Not_found (* XXX(dinosaure): we miss something! *)
  else
    let* prefix, prefix_count = get_prefix addr in
    if prefix_count + level < depth
    then return (depth - level) (* XXX(dinosaure): optimistic match *)
    else if prefix_count > 0
    then
      (* XXX(dinosaure): this case appears when [prefix_count > 0]
         and [prefix_count + level < depth]. This case is possible
         because [depth = length(prefix) = min prefix_count _prefix]!

         That means that [depth] does not include all of the [prefix_count]
         but only a part of it (only what we can save into [prefix]). *)
      let idx = (level + prefix_count) - depth
      and max = min prefix_count _prefix in
      _check_prefix ~key ~key_len ~prefix ~level idx max ;

      if prefix_count > _prefix
      then return (max + (prefix_count - _prefix)) (* XXX(dinosaure): optimistic match *)
      else return (- max) (* prefix_count <= _prefix + level >= depth *)
    else return 0 (* prefix_count:0 + level >= depth *)

let memcmp a b =
  if String.length a <> String.length b then raise Not_found ;
  let len = String.length a in
  let len0 = len land 3 in
  let len1 = len asr 2 in
  for i = 0 to len1 - 1 do
    let i = i * 4 in
    if String.unsafe_get_uint32 a i <> String.unsafe_get_uint32 b i
    then raise Not_found;
  done ;
  for i = 0 to len0 - 1 do
    let i = (len1 * 4) + i in
    if a.[i] <> b.[i] then raise Not_found ;
  done
;;

(* XXX(dinosaure):

   [  ]: match
   [  ]: no match
   [  ]: skipped level *)

let ( >>| ) x f = Bind (x, return <.> f)

type pessimistic =
  | Match of { level : int }
  | Skipped_level
  | No_match of { non_matching_key : char
                ; non_matching_prefix : string
                ; level : int }

(* XXX(dinosaure): [level] keeps 2 int31 values. It is initialised with:
   [level = (level lsl 31) lor level]. When we [succ level], we update only
   the right part - and the left part becomes the initial **constant** value
   of [level] while the loop.

   /!\ Overflow is possible... *)

let rec _check_prefix_pessimistic ~key ~minimum ~prefix ~prefix_count ~level idx max =
  if idx = max then return (Match { level= level land 0x7fffffff })
  else
    let* chr =
      (* XXX(dinosaure): note that a minimum, a leaf is, in ANYWAY,
         a constant value. It's why we can keep it as a [addr Lazy.t]
         without any trouble. *)
      if idx >= _prefix
      then Lazy.force minimum >>| fun key' -> key'.[idx]
      else return prefix.[idx] in
    if chr <> key.[level land 0x7fffffff]
    then
      let non_matching_key = chr in
      let* non_matching_prefix =
        if prefix_count > _prefix
        then
          let res = Bytes.make _prefix '\000' in
          let* key = Lazy.force minimum in
          let len = min (prefix_count - ((level land 0x7fffffff) - (level asr 31)) - 1) _prefix in
          if len > 0 then Bytes.blit_string key ((level land 0x7fffffff) + 1) res 0 len ;
          return (Bytes.unsafe_to_string res)
        else
          let res = Bytes.make _prefix '\000' in
          if prefix_count - idx - 1 > 0 then Bytes.blit_string prefix (idx + 1) res 0 (prefix_count - idx - 1) ;
          return (Bytes.unsafe_to_string res) in
      return (No_match { non_matching_key
                       ; non_matching_prefix
                       ; level= level land 0x7fffffff })
    else _check_prefix_pessimistic ~key ~minimum ~prefix ~prefix_count ~level:(succ level) (succ idx) max

let check_prefix_pessimistic (addr : ([ `Wr | `Rd ] as 'a) Addr.t) ~key level =
  let* prefix, prefix_count = get_prefix addr in
  let* depth = get Addr.(addr + _header_depth) Value.beint31 in
  if prefix_count + level < depth
  then return Skipped_level
  else if prefix_count > 0
  then
    let idx = (level + prefix_count) - depth in
    let max = prefix_count in
    let minimum = Lazy.from_fun @@ fun () ->
      let* leaf = minimum addr in
      get leaf Value.c_string in
    _check_prefix_pessimistic ~key ~minimum
      ~prefix ~prefix_count
      ~level:((level lsl 31) lor level) idx max
  else return (Match { level })
      (* XXX(dinosaure): even if [level] still is the same,
         it seems that [_check_prefix_pessimistic] can return with
         an other [level] value. *)

let rec _lookup (node : [ `Rd ] Addr.t) ~key ~key_len ~optimistic_match level =
  let* res = check_prefix node ~key ~key_len level in
  let optimistic_match = if res > 0 then true else optimistic_match in
  let level = level + (abs res) in
  if key_len < level then raise Not_found ;
  let* node = find_child node key.![level] in
  if Addr.is_null node then raise Not_found ;
  if (node :> int) land 1 = 1 (* XXX(dinosaure): it is a leaf. *)
  then ( let leaf = Leaf.prj (Addr.unsafe_to_leaf node) in
         if level < key_len - 1 || optimistic_match
         then
           let* key' = get leaf Value.c_string in
           memcmp key key' ;
           let len = (String.length key + size_of_word) / size_of_word in (* padding *)
           let len = len * size_of_word in
           get Addr.(leaf + len) Value.beintnat
         else
           let len = (String.length key + size_of_word) / size_of_word in (* padding *)
           let len = len * size_of_word in
           get Addr.(leaf + len) Value.beintnat )
  else _lookup node ~key ~key_len ~optimistic_match (succ level)

let lookup node ~key ~key_len =
  _lookup node ~key ~key_len ~optimistic_match:false 0

let find addr key =
  let key_len = String.length key in
  lookup addr ~key ~key_len

[@@@warning "-37"]

type 'a succ = S : 'a succ
type zero = Z

type 'a node =
  | N4   : [ `Rd | `Wr ] Addr.t -> zero node
  | N16  : [ `Rd | `Wr ] Addr.t -> zero succ node
  | N48  : [ `Rd | `Wr ] Addr.t -> zero succ succ node
  | N256 : [ `Rd | `Wr ] Addr.t -> zero succ succ succ node

[@@@warning "+37"]

(***** ADD CHILD *****)

let add_child_n256 (N256 addr) k value =
  let* () = atomic_set ~memory_order:Release
      Addr.(addr + _header_length + (k * Addr.length))
      Value.addr_rd value in
  let* () = fetch_add
      Addr.(addr + _header_count)
      Value.beint16 1 in
  return true

let add_child_n48 (N48 addr) k value =
  let* compact_count = atomic_get Addr.(addr + _header_compact_count) Value.beint16 in
  if compact_count = 48
  then return false
  else
    let* () = atomic_set ~memory_order:Release
        Addr.(addr + _header_length + 256 + (compact_count * Addr.length))
        Value.addr_rd value in
    let* () = atomic_set ~memory_order:Release
        Addr.(addr + _header_length + k)
        Value.int8 compact_count in
    let* () = fetch_add
        Addr.(addr + _header_compact_count)
        Value.beint16 1 in
    let* () = fetch_add
        Addr.(addr + _header_count)
        Value.beint16 1 in
    return true

let add_child_n16 (N16 addr) k value =
  let* compact_count = atomic_get Addr.(addr + _header_compact_count) Value.beint16 in
  if compact_count = 16
  then return false
  else
    let* () = atomic_set ~memory_order:Release
        Addr.(addr + _header_length + 16 + (compact_count * Addr.length))
        Value.addr_rd value in
    let* () = atomic_set ~memory_order:Release
        Addr.(addr + _header_length + compact_count)
        Value.int8 (k lxor 128) in
    let* () = fetch_add
        Addr.(addr + _header_compact_count)
        Value.beint16 1 in
    let* () = fetch_add
        Addr.(addr + _header_count)
        Value.beint16 1 in
    return true

let add_child_n4 (N4 addr) k value =
  let* compact_count = atomic_get Addr.(addr + _header_compact_count) Value.beint16 in
  if compact_count = 4
  then return false
  else
    let* () = atomic_set ~memory_order:Release
        Addr.(addr + _header_length + 4 + (compact_count * Addr.length))
        Value.addr_rd value in
    let* () = atomic_set ~memory_order:Release
        Addr.(addr + _header_length + compact_count)
        Value.int8 k in
    let* () = fetch_add
        Addr.(addr + _header_compact_count)
        Value.beint16 1 in
    let* () = fetch_add
        Addr.(addr + _header_count)
        Value.beint16 1 in
    return true

let write_unlock addr =
  fetch_add Addr.(addr + _header_kind) Value.beintnat 0b10

let write_unlock_and_obsolete addr =
  fetch_add Addr.(addr + _header_kind) Value.beintnat 0b11

let is_obsolete version = (version land 1 = 1)

let _read_unlock_or_restart addr expected =
  let* value = atomic_get Addr.(addr + _header_kind) Value.beintnat in
  return (expected = value)
[@@inline]

(* XXX(dinosaure): spin-lock *)

let rec until_is_locked addr version =
  if version land 0b10 = 0b10
  then
    let* () = pause_intrinsic in
    let* version = atomic_get Addr.(addr + _header_kind) Value.beintnat in
    until_is_locked addr version
  else return version
[@@inline]

let rec write_lock_or_restart addr need_to_restart =
  let* version = atomic_get Addr.(addr + _header_kind) Value.beintnat in
  let* version = until_is_locked addr version in
  if is_obsolete version
  then ( need_to_restart := true ; return () )
  else
    let* res = compare_exchange ~weak:true Addr.(addr + _header_kind) Value.beintnat version (version + 0b10) in
    if not res then write_lock_or_restart addr need_to_restart else return ()
[@@inline]

let lock_version_or_restart addr version need_to_restart =
  if (version land 0b10 = 0b10)|| (version land 1 = 1)
  then ( need_to_restart := true ; return version)
  else
    let* set = compare_exchange Addr.(addr + _header_kind) Value.beintnat version (version + 0b10) in
    if set then return (version + 0b10) else ( need_to_restart := true ; return version )

(***** CHANGE/UPDATE CHILD *****)

(* XXX(dinosaure): may be do an optimisation pass on this part of the code.
   For example, in [_n4_update_child], [compact_count] should be loaded only
   one time. The check of [child] to see if it's not [NULL] can be deleted
   if our assumptions are rights.

   Should we protect [addr] with typed constructor? *)

let rec _n4_update_child addr k ptr i =
  let* compact_count = atomic_get Addr.(addr + _header_compact_count) Value.beint16 in
  if i < compact_count
  then
    let* key = atomic_get Addr.(addr + _header_length + i) Value.int8 in
    let* child = atomic_get Addr.(addr + _header_length + 4 + (i * Addr.length)) Value.addr_rd in
    if not (Addr.is_null child) && key = k
    then atomic_set Addr.(addr + _header_length + 4 + (i * Addr.length)) Value.addr_rd ptr
    else _n4_update_child addr k ptr (succ i)
  else assert false (* XXX(dinosaure): impossible or integrity problem! *)

let n4_update_child addr k ptr = _n4_update_child addr k ptr 0

let rec _n16_child_pos addr bitfield =
  if bitfield = 0 then return Addr.null
  else
    let p = ctz bitfield in
    let* child = atomic_get Addr.(addr + _header_length + 16 + (p * Addr.length)) Value.addr_rd in
    if not (Addr.is_null child) then return child
    else _n16_child_pos addr (bitfield lxor (1 lsl p))

let _n16_child_pos addr k =
  let* compact_count = atomic_get Addr.(addr + _header_compact_count) Value.beint16 in
  let* keys = atomic_get Addr.(addr + _header_length) Value.beint128 in
  let bitfield = n16_get_child compact_count k keys in
  _n16_child_pos addr bitfield

let n16_update_child addr k ptr =
  let* value = _n16_child_pos addr k in
  let value = Addr.of_int_wronly (value :> int) in (* XXX(dinosaure): unsafe! *)
  atomic_set ~memory_order:Release value Value.addr_rd ptr

let n48_update_child addr k ptr =
  let* idx = atomic_get Addr.(addr + _header_length + k) Value.int8 in
  atomic_set ~memory_order:Release Addr.(addr + _header_length + 256 + (idx * Addr.length)) Value.addr_rd ptr

let n256_update_child addr k ptr =
  atomic_set ~memory_order:Release
    Addr.(addr + _header_length + (k * Addr.length)) Value.addr_rd ptr

let update_child addr k ptr =
  let* ty = get_type addr in
  Log.debug (fun m -> m "Update child %02x" k) ;
  match ty with
  | 0 -> n4_update_child addr k ptr
  | 1 -> n16_update_child addr k ptr
  | 2 -> n48_update_child addr k ptr
  | 3 -> n256_update_child addr k ptr
  | _ -> assert false

(***** ALLOCATION *****)

let n4 addr = N4 addr
let n16 addr = N16 addr
let n48 addr = N48 addr
let n256 addr = N256 addr

let ( >>| ) x f = Bind (x, return <.> f)

let _count = String.make 2 '\000'
let _compact_count = String.make 2 '\000'

let _n4_ks = String.make 4 '\000'
let _n4_vs = String.concat "" (List.init 4 (const string_of_null_addr))

let alloc_n4 ~prefix:p ~prefix_count ~level =
  let prefix = Bytes.make 4 '\000' in
  Bytes.blit_string p 0 prefix 0 (min _prefix (String.length p)) ;
  let prefix_count = beint31_to_string prefix_count in
  let k = beintnat_to_string ((_n4_kind lsl _bits_kind) lor 0b100) in
  let l = beint31_to_string level in
  allocate [ Bytes.unsafe_to_string prefix; prefix_count; k; l; _count; _compact_count; _n4_ks; _n4_vs ]
    ~len:(_header_length + 4 + (4 * Addr.length)) >>| n4

let _n16_ks = String.make 16 '\000'
let _n16_vs = String.concat "" (List.init 16 (const string_of_null_addr))

let alloc_n16 ~prefix:p ~prefix_count ~level =
  let prefix = Bytes.make 4 '\000' in
  Bytes.blit_string p 0 prefix 0 (min _prefix (String.length p)) ;
  let prefix_count = beint31_to_string prefix_count in
  let k = beintnat_to_string ((_n16_kind lsl _bits_kind) lor 0b100) in
  let l = beint31_to_string level in
  allocate [ Bytes.unsafe_to_string prefix; prefix_count; k; l; _count; _compact_count; _n16_ks; _n16_vs ]
    ~len:(_header_length + 16 + (16 * Addr.length)) >>| n16

let _n48_ks = String.make 256 '\048'
let _n48_vs = String.concat "" (List.init 48 (const string_of_null_addr))

let alloc_n48 ~prefix:p ~prefix_count ~level =
  let prefix = Bytes.make 4 '\000' in
  Bytes.blit_string p 0 prefix 0 (min _prefix (String.length p)) ;
  let prefix_count = beint31_to_string prefix_count in
  let k = beintnat_to_string ((_n48_kind lsl _bits_kind) lor 0b100) in
  let l = beint31_to_string level in
  allocate [ Bytes.unsafe_to_string prefix; prefix_count; k; l; _count; _compact_count; _n48_ks; _n48_vs ]
    ~len:(_header_length + 256 + (48 * Addr.length)) >>| n48

let _n256_vs = String.concat "" (List.init 256 (const string_of_null_addr))

let alloc_n256 ~prefix:p ~prefix_count ~level =
  let prefix = Bytes.make 4 '\000' in
  Bytes.blit_string p 0 prefix 0 (min _prefix (String.length p)) ;
  let prefix_count = beint31_to_string prefix_count in
  let k = beintnat_to_string ((_n256_kind lsl _bits_kind) lor 0b100) in
  let l = beint31_to_string level in
  allocate [ Bytes.unsafe_to_string prefix; prefix_count; k; l; _count; _compact_count; _n256_vs ]
    ~len:(_header_length + (256 * Addr.length)) >>| n256

(***** COPY CHILD <N0, N1> (assert (sizeof(N0) <= sizeof(N1))) *****)

let rec _copy_n4_into_n16 ~compact_count n4 n16 i =
  if i = compact_count
  then return ()
  else
    let* value = atomic_get Addr.(n4 + _header_length + 4 + (i * Addr.length)) Value.addr_rd in
    match Addr.is_null value with
    | true  -> _copy_n4_into_n16 ~compact_count n4 n16 (succ i)
    | false ->
      let* key = atomic_get Addr.(n4 + _header_length + i) Value.int8 in
      let* _   = add_child_n16 n16 key value in (* XXX(dinosaure): assert (_ = true); *)
      Log.debug (fun m -> m "<N4 -> N16> copy %02x (%016x)" key (value :> int)) ;
      _copy_n4_into_n16 ~compact_count n4 n16 (succ i)

let copy_n4_into_n16 (N4 n4) n16 =
  let* compact_count = atomic_get Addr.(n4 + _header_compact_count) Value.beint16 in
  _copy_n4_into_n16 ~compact_count n4 n16 0

(* XXX(dinosaure): [copy_n4_into_n4] is called when:
   - [compact_count = 4]
   - [count <= 3]

   Such case appears about deletion when we decrease only [count]. So we must
   scan any objects & copy them into the new node if they are not equal to [null]. *)

let rec _copy_n4_into_n4 nx ny i =
  if i = 4
  then return ()
  else
    let* value = atomic_get Addr.(nx + _header_length + 4 + (i * Addr.length)) Value.addr_rd in
    match Addr.is_null value with
    | true  -> _copy_n4_into_n4 nx ny (succ i)
    | false ->
      let* key = atomic_get Addr.(nx + _header_length + i) Value.int8 in
      let* _   = add_child_n4 ny key value in (* XXX(dinosaure): assert (_ = true); *)
      _copy_n4_into_n4 nx ny (succ i)

let copy_n4_into_n4 (N4 nx) ny = _copy_n4_into_n4 nx ny 0

let rec _copy_n16_into_n48 ~compact_count n16 n48 i =
  if i = compact_count
  then return ()
  else
    let* value = atomic_get Addr.(n16 + _header_length + 16 + (i * Addr.length)) Value.addr_rd in
    match Addr.is_null value with
    | true  -> _copy_n16_into_n48 ~compact_count n16 n48 (succ i)
    | false ->
      let* key = atomic_get Addr.(n16 + _header_length + i) Value.int8 in
      let* _   = add_child_n48 n48 (key lxor 128) value in (* XXX(dinosaure): assert (_ = true); *)
      _copy_n16_into_n48 ~compact_count n16 n48 (succ i)

let copy_n16_into_n48 (N16 n16) n48 =
  let* compact_count = atomic_get Addr.(n16 + _header_compact_count) Value.beint16 in
  _copy_n16_into_n48 ~compact_count n16 n48 0

let rec _copy_n16_into_n16 nx ny i =
  if i = 16
  then return ()
  else
    let* value = atomic_get Addr.(nx + _header_length + 16 + (i * Addr.length)) Value.addr_rd in
    match Addr.is_null value with
    | true  -> _copy_n16_into_n16 nx ny (succ i)
    | false ->
      let* key = atomic_get Addr.(nx + _header_length + i) Value.int8 in
      let* _   = add_child_n16 ny (key lxor 128) value in (* XXX(dinosaure): ssert (_ = true); *)
      _copy_n16_into_n16 nx ny (succ i)

let copy_n16_into_n16 (N16 nx) ny = _copy_n16_into_n16 nx ny 0

let rec _copy_n48_into_n256 n48 n256 k =
  if k = 256 then return ()
  else
    let* index = atomic_get Addr.(n48 + _header_length + k) Value.int8 in
    match index with
    | 48  -> _copy_n48_into_n256 n48 n256 (succ k)
    | _ ->
      let* value = atomic_get Addr.(n48 + _header_length + 256 + (index * Addr.length)) Value.addr_rd in
      let* _ = add_child_n256 n256 k value in (* XXX(dinosaure): assert (_ = true); *)
      _copy_n48_into_n256 n48 n256 (succ k)

let copy_n48_into_n256 (N48 n48) n256 =
  _copy_n48_into_n256 n48 n256 0

let rec _copy_n48_into_n48 nx ny k =
  if k = 256 then return ()
  else
    let* index = atomic_get Addr.(nx + _header_length + k) Value.int8 in
    match index with
    | 48 -> _copy_n48_into_n48 nx ny (succ k)
    | _ ->
      let* value = atomic_get Addr.(nx + _header_length + 256 + (index * Addr.length)) Value.addr_rd in
      let* _ = add_child_n48 ny k value in (* XXX(dinosaure): assert (_ = true); *)
      _copy_n48_into_n48 nx ny (succ k)

let copy_n48_into_n48 (N48 nx) ny = _copy_n48_into_n48 nx ny 0

let _insert_grow_n4_n16 (N4 addr as n4) p k kp value need_to_restart =
  let* inserted = add_child_n4 n4 k value in
  if inserted then write_unlock addr
  else
    ( Log.debug (fun m -> m "We must grow the N4 node to a N16 node")
    ; let* prefix, prefix_count = get_prefix addr in
      let* level = get Addr.(addr + _header_depth) Value.beint31 in
      let* N16 addr' as n16 = alloc_n16 ~prefix ~prefix_count ~level in
      Log.debug (fun m -> m "Copy N4 children into new N16 node")
    ; let* () = copy_n4_into_n16 n4 n16 in
      let* _  = add_child_n16 n16 k value in (* XXX(dinosaure): assert (_ = true); *)
      let* () = write_lock_or_restart p need_to_restart in
      if !need_to_restart
      then ( let* () = delete addr' (_header_length + 16 + (Addr.length * 16)) in write_unlock addr )
      else
        let* () = update_child p kp (Addr.to_rdonly addr') in
        let* () = write_unlock p in
        let* () = write_unlock_and_obsolete addr in
        collect addr (_header_length + 4 + (Addr.length * 4)) )

let _insert_grow_n16_n48 (N16 addr as n16) p k kp value need_to_restart =
  let* inserted = add_child_n16 n16 k value in
  if inserted then write_unlock addr
  else
    let* prefix, prefix_count = get_prefix addr in
    let* level = get Addr.(addr + _header_depth) Value.beint31 in
    let* N48 addr' as n48 = alloc_n48 ~prefix ~prefix_count ~level in
    let* () = copy_n16_into_n48 n16 n48 in
    let* _  = add_child_n48 n48 k value in (* XXX(dinosaure): assert (_ = true); *)
    let* () = write_lock_or_restart p need_to_restart in
    if !need_to_restart
    then ( let* () = delete addr' (_header_length + 256 + (Addr.length * 48)) in write_unlock addr )
    else
      let* () = update_child p kp (Addr.to_rdonly addr') in
      let* () = write_unlock p in
      let* () = write_unlock_and_obsolete addr in
      collect addr (_header_length + 16 + (Addr.length * 16))

let _insert_grow_n48_n256 (N48 addr as n48) p k kp value need_to_restart =
  let* inserted = add_child_n48 n48 k value in
  if inserted then write_unlock addr
  else
    let* prefix, prefix_count = get_prefix addr in
    let* level = get Addr.(addr + _header_depth) Value.beint31 in
    let* N256 addr' as n256 = alloc_n256 ~prefix ~prefix_count ~level in
    let* () = copy_n48_into_n256 n48 n256 in
    let* _  = add_child_n256 n256 k value in (* XXX(dinosaure): assert (_ = true); *)
    let* () = write_lock_or_restart p need_to_restart in
    if !need_to_restart
    then ( let* () = delete addr' (_header_length + 256 + (Addr.length * 256)) in write_unlock addr )
    else
      let* () = update_child p kp (Addr.to_rdonly addr') in
      let* () = write_unlock p in
      let* () = write_unlock_and_obsolete addr in
      collect addr (_header_length + 256 + (Addr.length * 48))

let insert_compact_n4 (N4 addr as n4) p k kp value need_to_restart =
  let* prefix, prefix_count = get_prefix addr in
  let* level = get Addr.(addr + _header_depth) Value.beint31 in
  let* N4 addr' as n4' = alloc_n4 ~prefix ~prefix_count ~level in
  let* () = copy_n4_into_n4 n4 n4' in
  let* _  = add_child_n4 n4' k value in (* XXX(dinosaure): assert (_ = true); *)
  let* () = write_lock_or_restart p need_to_restart in
  if !need_to_restart
  then ( let* () = delete addr' (_header_length + 4 + (Addr.length * 4)) in write_unlock addr )
  else
    let* () = update_child p kp (Addr.to_rdonly addr') in
    let* () = write_unlock p in
    let* () = write_unlock_and_obsolete addr in
    collect addr'(_header_length + 4 + (Addr.length * 4))

let insert_compact_n16 (N16 addr as n16) p k kp value need_to_restart =
  let* prefix, prefix_count = get_prefix addr in
  let* level = get Addr.(addr + _header_depth) Value.beint31 in
  let* N16 addr' as n16' = alloc_n16 ~prefix ~prefix_count ~level in
  let* () = copy_n16_into_n16 n16 n16' in
  let* _  = add_child_n16 n16' k value in (* XXX(dinosaure): assert (_ = true); *)
  let* () = write_lock_or_restart p need_to_restart in
  if !need_to_restart
  then ( let* () = delete addr' (_header_length + 16 + (Addr.length * 16)) in write_unlock addr )
  else
    let* () = update_child p kp (Addr.to_rdonly addr') in
    let* () = write_unlock p in
    let* () = write_unlock_and_obsolete addr in
    collect addr (_header_length + 16 + (Addr.length * 16))

let insert_compact_n48 (N48 addr as n48) p k kp value need_to_restart =
  let* prefix, prefix_count = get_prefix addr in
  let* level = get Addr.(addr + _header_depth) Value.beint31 in
  let* N48 addr' as n48' = alloc_n48 ~prefix ~prefix_count ~level in
  let* () = copy_n48_into_n48 n48 n48' in
  let* _  = add_child_n48 n48' k value in (* XXX(dinosaure): assert (_ = true); *)
  let* () = write_lock_or_restart p need_to_restart in
  if !need_to_restart
  then ( let* () = delete addr' (_header_length + 256 + (Addr.length * 48)) in write_unlock addr )
  else
    let* () = update_child p kp (Addr.to_rdonly addr') in
    let* () = write_unlock p in
    let* () = write_unlock_and_obsolete addr in
    collect addr (_header_length + 256 + (Addr.length * 48))

let insert_and_unlock n p k kp value need_to_restart =
  let* ty = get_type n in
  match ty with
  | 0 ->
    let* compact_count = atomic_get Addr.(n + _header_compact_count) Value.beint16 in
    let* count = atomic_get Addr.(n + _header_count) Value.beint16 in
    Log.debug (fun m -> m "insert %02x into N4 (compact_count: %d, count: %d)" k compact_count count) ;
    if compact_count = 4 && count <= 3
    then insert_compact_n4 (N4 n) p k kp value need_to_restart
    else _insert_grow_n4_n16 (N4 n) p k kp value need_to_restart
  | 1 ->
    let* compact_count = atomic_get Addr.(n + _header_compact_count) Value.beint16 in
    let* count = atomic_get Addr.(n + _header_count) Value.beint16 in
    if compact_count = 16 && count <= 14
    then insert_compact_n16 (N16 n) p k kp value need_to_restart
    else _insert_grow_n16_n48 (N16 n) p k kp value need_to_restart
  | 2 ->
    let* compact_count = atomic_get Addr.(n + _header_compact_count) Value.beint16 in
    let* count = atomic_get Addr.(n + _header_count) Value.beint16 in
    if compact_count = 48 && count <> 48
    then insert_compact_n48 (N48 n) p k kp value need_to_restart
    else _insert_grow_n48_n256 (N48 n) p k kp value need_to_restart
  | 3 ->
    let* _  = add_child_n256 (N256 n) k value in
    let* () = write_unlock n in
    return ()
  | _ -> assert false

exception Duplicate

let check_or_raise_duplicate ~level:off a b =
  if String.length a = String.length b
  then ( let idx = ref (String.length a - 1) in
         while !idx >= off && a.[!idx] = b.[!idx] do decr idx done ;
         if !idx < off then raise Duplicate )

let rec insert root key leaf =
  let rec restart () = insert root key leaf

  (* XXX(dinosaure): with [multicore] (eg. ['a t = 'a]), it should be posible to
     raise an exception [Restart] and simulate a [goto] as ROWEX explains.
     However, if we took the monadic-view of ['a t], [Restart] will leak.

     So we call [restart] and ensure that the call is tail-recursive (and can be
     optimised by OCaml). Then, we compile with [-unbox-closures] to avoid
     allocation on this area - but we need to introspect such optimisation. *)

  and _insert (node : [ `Rd | `Wr ] Addr.t) parent pk level =
    let need_to_restart = ref false in
    let* version = get_version node in
    let* res = check_prefix_pessimistic node ~key level in
    ( match res with
    | Skipped_level -> restart ()
    | No_match { non_matching_key; non_matching_prefix; level= level'; } ->
      Log.debug (fun m -> m "check_prefix_pessimistic %016x ~key:%s %d : \
                             No_match { non_matching_key: %c; non_matching_prefix: %S; level: %d }"
                            (node :> int) key level non_matching_key non_matching_prefix level') ;
      if level' >= String.length key then raise Duplicate ;
      let* _version = lock_version_or_restart node version need_to_restart in
      if !need_to_restart then (restart[@tailcall]) () else
      let* prefix, _ = get_prefix node in
      let* N4 addr as n4 = alloc_n4 ~prefix ~prefix_count:(level - level') ~level:level' in
      let* _             = add_child_n4 n4 (Char.code key.[level']) leaf in
      let* _             = add_child_n4 n4 (Char.code non_matching_key) (Addr.to_rdonly node) in
      let* ()            = write_lock_or_restart parent need_to_restart in
      if !need_to_restart
      then
        let* () = delete addr (_header_length + 4 + (Addr.length * 4)) in
        let* () = write_unlock node in
        (restart[@tailcall]) ()
      else
        let* () = update_child parent pk (Addr.to_rdonly addr) in
        let* () = write_unlock parent in
        let* _, prefix_count = get_prefix node in
        let* () = set_prefix node ~prefix:non_matching_prefix
            ~prefix_count:(prefix_count - ((level' - level) + 1)) in
        let* () = write_unlock node in
        return ()
    | Match { level= level' } ->
      Log.debug (fun m -> m "check_prefix_pessimistic %016x ~key:%s %d : Match { %d }"
                            (node :> int) key level level') ;
      let level = level' in
      let* next = find_child node key.[level] in
      if Addr.is_null next
      then
        let* _version = lock_version_or_restart node version need_to_restart in
        if !need_to_restart then (restart[@tailcall]) () else
        ( let* () = insert_and_unlock node parent (Char.code key.[level]) pk leaf
                      need_to_restart in
          if !need_to_restart then (restart[@tailcall]) () else
            return () )
      else if (next :> int) land 1 = 1
      then
        let* key' = get (Leaf.prj (Addr.unsafe_to_leaf next)) Value.c_string in
        check_or_raise_duplicate ~level:(level + 1) key key' ;
        (* XXX(dinosaure): in the C impl., this check does **not** exists but:
           - create ()
           - insert "foo" 0
           - insert "foo" 1
           seems to work. So, the check try find the diff between [key] and [key']
           from the end of these strings. The worst case is when [key = key'] of course
           but we should assume that the user does not want to insert several times
           the same key. *) 
        let* _version = lock_version_or_restart node version need_to_restart in
        if !need_to_restart then (restart[@tailcall]) () else
        ( let prefix = Bytes.make _prefix '\000' in
          let prefix_count = ref 0 in
          let top = min (String.length key - (level + 1)) (String.length key' - (level + 1)) in
          while !prefix_count < top && key.[level + 1 + !prefix_count] = key'.[level + 1 + !prefix_count]
          do Bytes.set prefix !prefix_count key.[level + 1] ; incr prefix_count done ;
          let* N4 addr as n4 = alloc_n4 ~prefix:(Bytes.unsafe_to_string prefix)
              ~prefix_count:!prefix_count ~level:(level + 1 + !prefix_count) in
          (* XXX(dinosaure): Imagine you add "foo" and "fo" into an empty tree (see [ctor]),
             we have: 1) a prefix "o" 2) an alteration between the end of "fo" and the last "o"
             of "foo". In that case, we must have an ~illegal~ access on these strings - fortunately,
             OCaml always pads a string with at least, one '\000'. So even if it seems an unsafe
             access, it is ~safe~ in this **specific** context. *)
          let* _   = add_child_n4 n4 (Char.code key.![level + 1 + !prefix_count]) leaf in
          let* _   = add_child_n4 n4 (Char.code key'.![level + 1 + !prefix_count]) next in
          let* _   = update_child node (Char.code key.[level]) (Addr.to_rdonly addr) in
          let* _   = write_unlock node in
          return () )
      else _insert (Addr.of_int_rdwr (next :> int)) node (Char.code key.[level]) (succ level) ) in

  (* XXX(dinosaure): [ctor] creates a [N256] node on the [root]. So, the case to
     enlarge the current [root] node **can not** appears and the "parent" should
     not be set. In that case, it's ~safe~ to consider at the beginning [parent]
     as a [Addr.null] address.

     NOTE(dinosaure): this is the **only** case where we need to /cast/
     [Addr.null] to a [[ `Rd | `Wr ] Addr.t] value - to have a write access to
     [Addr.null]. According to the comment below, this write access should not
     be used to write something into [Addr.null] but we need to play the game of
     the type-system. *)

  _insert root Addr.(of_int_rdwr (null :> int)) 0 0

let ctor () =
  let* N256 addr = alloc_n256 ~prefix:"" ~prefix_count:0 ~level:0 in
  return (addr)

let insert root key value =
  let len = (String.length key + size_of_word) / size_of_word in (* padding *)
  let len = len * size_of_word in
  let pad = String.make (len - String.length key) '\000' in
  let value = beintnat_to_string value in
  let* leaf = allocate [ key; pad; value ] in
  insert root key (Addr.unsafe_of_leaf (Leaf.inj leaf))
