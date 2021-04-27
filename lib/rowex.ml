let () = Printexc.record_backtrace true

let (.![]) = String.unsafe_get
(* XXX(dinosaure): see [art.ml] about this unsafe access. *)

external ( <= ) : 'a -> 'a -> bool = "%lessequal"
let ( <= ) (x : int) y = x <= y [@@inline]
let min (a : int) b = if a <= b then a else b [@@inline]

type key = string

let key : string -> key = fun key ->
  if String.contains key '\000' then invalid_arg "Invalid key" ; key

external unsafe_key : string -> key = "%identity"

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

let leintnat_to_string v =
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

let leint31_to_string v =
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
   significant bit to 1 is a leaf, otherwise, it is a node. Such layout is
   possible when **any** addresses are an even absolute number! To ensure that,
   the only value which can introduce an odd shift is a C string (something
   which terminates by a '\000') but we ensure that a C string has a "pad" of
   '\000' such as OCaml does.

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

  let prj x = x lsr 1 [@@inline always]
  let inj x = (x lsl 1) lor 1 [@@inline always]
end

and Addr : sig
  type 'a t = private int constraint 'a = [< `Rd | `Wr ] [@@immediate]

  val length : int

  val null : [ `Rd ] t
  val is_null : 'a t -> bool

  external of_int_rdonly : int -> [ `Rd ] t = "%identity"
  external of_int_wronly : int -> [ `Wr ] t = "%identity"
  external of_int_rdwr : int -> [ `Rd | `Wr ] t = "%identity"

  external to_wronly : [> `Wr ] t -> [ `Wr ] t = "%identity"
  external to_rdonly : [> `Rd ] t -> [ `Rd ] t = "%identity"

  external unsafe_to_leaf : 'a t -> Leaf.t = "%identity"
  external unsafe_of_leaf : Leaf.t -> 'a t = "%identity"
  external unsafe_to_int : _ t -> int = "%identity"

  val ( + ) : 'a t -> int -> 'a t
end = struct
  type 'a t = int constraint 'a = [< `Rd | `Wr ]

  let length = Sys.word_size / 8

  let null = 1 lsl (Sys.word_size - 2)
  let is_null x = x = null [@@inline always]

  external of_int_rdonly : int -> [ `Rd ] t = "%identity"
  external of_int_wronly : int -> [ `Wr ] t = "%identity"
  external of_int_rdwr : int -> [ `Rd | `Wr ] t = "%identity"

  external to_wronly : [> `Wr ] t -> [ `Wr ] t = "%identity"
  external to_rdonly : [> `Rd ] t -> [ `Rd ] t = "%identity"

  external unsafe_to_leaf : 'a t -> Leaf.t = "%identity"
  external unsafe_of_leaf : Leaf.t -> 'a t = "%identity"
  external unsafe_to_int : _ t -> int = "%identity"

  let ( + ) addr v = addr + v [@@inline always]
end

let string_of_null_addr = leintnat_to_string (Addr.null :> int)

type ('c, 'a) value =
  | Int8     : ([ `Atomic ], int) value
  | LEInt    : ([ `Atomic ], int) value
  | LEInt16  : ([ `Atomic ], int) value
  | LEInt31  : ([ `Atomic ], int) value
  | LEInt64  : ([ `Atomic ], int64) value
  | LEInt128 : ([ `Atomic ], string) value
  (* XXX(dinosaure): a Int128 does not exist in OCaml, so we load it into a
       simple (big-endian) [string]. However, the access to the value must
       be atomic and be saved into a string then. *)
  | Addr_rd  : ([ `Atomic ], [ `Rd ] Addr.t) value
  | C_string : ([ `Non_atomic ], string) value

type 'c memory_order =
  | Relaxed : [< `Rd | `Wr ] memory_order
  | Seq_cst : [< `Rd | `Wr ] memory_order
  | Release : [< `Wr ] memory_order
  | Acq_rel : [< `Wr | `Rd ] memory_order
  | Acquire : [< `Rd ] memory_order

module type S = sig
  type 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t

  val atomic_get : ?memory_order:[< `Rd ] memory_order -> [> `Rd ] Addr.t ->
    ([ `Atomic ], 'a) value -> 'a t
  val atomic_set : ?memory_order:[< `Wr ] memory_order -> [> `Wr ] Addr.t ->
    ([ `Atomic ], 'a) value -> 'a -> unit t
  val fetch_add  : ?memory_order:[< `Rd | `Wr ] memory_order ->
    [> `Rd | `Wr ] Addr.t -> ([ `Atomic ], int) value -> int -> int t
  val fetch_or   : ?memory_order:[< `Rd | `Wr ] memory_order ->
    [> `Rd | `Wr ] Addr.t -> ([ `Atomic ], int) value -> int -> int t
  val fetch_sub  : ?memory_order:[< `Rd | `Wr ] memory_order ->
    [> `Rd | `Wr ] Addr.t -> ([ `Atomic ], int) value -> int -> int t

  val compare_exchange :
    ?m0:[< `Rd | `Wr ] memory_order ->
    ?m1:[< `Rd | `Wr ] memory_order ->
    ?weak:bool ->
    [> `Rd | `Wr ] Addr.t ->
    ([ `Atomic ], 'a) value ->
    'a ref -> 'a -> bool t

  val pause_intrinsic : unit t

  val get : [> `Rd ] Addr.t -> ('c, 'a) value -> 'a t

  val allocate : kind:[ `Leaf | `Node ] -> ?len:int -> string list ->
    [ `Rd | `Wr ] Addr.t t
  val delete : _ Addr.t -> int -> unit t
  val collect : _ Addr.t -> len:int -> uid:int -> unit t

  val rdtsc : int t

  val clflush : [ `Wr ] Addr.t -> unit t
  val sfence : unit t

  val stream_int : [ `Wr ] Addr.t -> int -> unit t
  (* XXX(dinosaure): [stream_*] means that we store the integer at the given
     address using a _non-temporal hint_ to minimize cache pollution. Such
     usage is to ensure that what we want to write is really written and it is
     not cached. [stream_*] uses the cache **only** if the given [addr] is in
     the cache. In our context, [stream_*] is used with [clflush] to ensure
     a real write.

     TODO(dinosaure): abstract it (as [atomic_{get,set}])? However, we should
     use such operation ONLY for address. *)
end

(*
type 'a t =
  | Atomic_get : [< `Rd ] memory_order * [> `Rd ] Addr.t * ([ `Atomic ], 'a) value -> 'a t
  | Atomic_set : [< `Wr ] memory_order * [> `Wr ] Addr.t * ([ `Atomic ], 'a) value * 'a -> unit t
  | Fetch_add  : [< `Rd | `Wr ] memory_order * [> `Rd | `Wr ] Addr.t * ([ `Atomic ], int) value * int -> int t
  | Fetch_or   : [< `Rd | `Wr ] memory_order * [> `Rd | `Wr ] Addr.t * ([ `Atomic ], int) value * int -> int t
  | Fetch_sub  : [< `Rd | `Wr ] memory_order * [> `Rd | `Wr ] Addr.t * ([ `Atomic ], int) value * int -> int t
  | Pause_intrinsic : unit t
  | Compare_exchange : [> `Rd | `Wr ] Addr.t *
                       ([ `Atomic ], 'a) value * 'a ref * 'a * bool * [< `Rd | `Wr ] memory_order
                       * [< `Rd | `Wr ] memory_order -> bool t
  | Get : [> `Rd ] Addr.t * ('c, 'a) value -> 'a t
  | Allocate : [ `Node | `Leaf ] * string list * int -> [ `Rd | `Wr ] Addr.t t
  | Delete : _ Addr.t * int -> unit t
  | Collect : _ Addr.t * int * int -> unit t
  | Bind : 'a t * ('a -> 'b t) -> 'b t
  | Return : 'a -> 'a t
*)

type 'a fmt = Format.formatter -> 'a -> unit

let pf ppf fmt = Format.fprintf ppf fmt

let pp_memory_order : type a. a memory_order fmt = fun ppf -> function
  | Seq_cst -> pf ppf "seq_cst"
  | Release -> pf ppf "release"
  | Relaxed -> pf ppf "relaxed"
  | Acq_rel -> pf ppf "acquire-release"
  | Acquire -> pf ppf "acquire"

let pp_value : type c a. (c, a) value fmt = fun ppf -> function
  | LEInt -> pf ppf "leintnat"
  | LEInt31 -> pf ppf "leint31"
  | LEInt16 -> pf ppf "leint16"
  | LEInt64 -> pf ppf "leint64"
  | LEInt128 -> pf ppf "leint128"
  | Int8 -> pf ppf "int8"
  | Addr_rd -> pf ppf "addr"
  | C_string -> pf ppf "c_string"

let fmt fmt = fun ppf -> pf ppf fmt

let pp_of_value : type c a. (c, a) value -> a fmt = function
  | LEInt -> fun ppf v -> if v < 0  then pf ppf "%16x" v else pf ppf "%10d" v
  | LEInt31 -> fmt "%10d"
  | LEInt16 -> fmt "%5d"
  | LEInt64 -> fmt "%19Ld"
  | LEInt128 -> fmt "%S"
  | Int8 -> fmt "%3d"
  | Addr_rd -> fun ppf addr -> pf ppf "%016x" (addr :> int)
  | C_string -> fmt "%S"

(*
let pp : type a. a t fmt = fun ppf -> function
  | Atomic_get (m, addr, v) ->
     pf ppf "atomic_get %016x %a : %a" (addr :> int) pp_memory_order m pp_value v
  | Atomic_set (m, addr, v, x) ->
     pf ppf "atomic_set %016x %a (%a : %a)" (addr :> int) pp_memory_order m (pp_of_value v) x pp_value v
  | Fetch_add (m, addr, v, x) ->
     pf ppf "fetch_add  %016x %a (%a : %a)" (addr :> int) pp_memory_order m (pp_of_value v) x pp_value v
  | Fetch_or (m, addr, v, x) ->
     pf ppf "fetch_or   %016x %a (%a : %a)" (addr :> int) pp_memory_order m (pp_of_value v) x pp_value v
  | Fetch_sub (m, addr, v, x) ->
     pf ppf "fetch_sub  %016x %a (%a : %a)" (addr :> int) pp_memory_order m (pp_of_value v) x pp_value v
  | Collect (addr, len, uid) ->
     pf ppf "collect    %016x %d %d" (addr :> int) len uid
  | Delete (addr, len) ->
     pf ppf "delete     %016x %d" (addr :> int) len
  | Get (addr, v) ->
     pf ppf "get        %016x         : %a" (addr :> int) pp_value v
  | Allocate (`Node, _, len) ->
     pf ppf "allocate %3d (node)" len
  | Allocate (`Leaf, _, len) ->
     pf ppf "allocate %3d (leaf)" len
  | Pause_intrinsic ->
     pf ppf "pause_intrinsic"
  | Compare_exchange (addr, v, x, y, weak, m0, m1) ->
    pf ppf "compare_exchange weak:%b %016x %a %a (%a : %a) (%a : %a)" weak (addr :> int)
       pp_memory_order m0
       pp_memory_order m1
       (pp_of_value v) !x pp_value v
       (pp_of_value v) y pp_value v
  | Bind (Allocate (_, _, len), _) ->
     pf ppf "allocate %d byte(s) >>= fun _ ->" len
  | Bind _ -> pf ppf ">>="
  | Return _ -> pf ppf "return *"
*)

module Value = struct
  let int8 = Int8
  let leint16 = LEInt16
  let leint31 = LEInt31
  let leintnat = LEInt
  let leint64 = LEInt64
  let leint128 = LEInt128
  let addr_rd = Addr_rd
  let c_string = C_string
end

let _cache_line_size = 64
let _write_latency_in_ns = 0
let _cpu_freq_mhz = 2100

(*
let ( let* ) x f = Bind (x, f)

let return x = Return x

let get addr value = Get (addr, value)

let atomic_get ?(memory_order= Seq_cst) addr k = Atomic_get (memory_order, addr, k)

let atomic_set ?(memory_order= Seq_cst) addr k v = Atomic_set (memory_order, addr, k, v)

let fetch_add  ?(memory_order= Seq_cst) addr k n = Fetch_add (memory_order, addr, k, n)
let fetch_or   ?(memory_order= Seq_cst) addr k n = Fetch_or  (memory_order, addr, k, n)
let fetch_sub  ?(memory_order= Seq_cst) addr k n = Fetch_sub (memory_order, addr, k, n)

let compare_exchange ?(m0= Seq_cst) ?(m1= Seq_cst) ?(weak= false) addr k expected desired =
  Compare_exchange (addr, k, expected, desired, weak, m0, m1)

let pause_intrinsic = Pause_intrinsic
*)

let ( <.> ) f g = fun x -> f (g x)

(*
let allocate ~kind ?len payloads =
  let len = match len with
    | Some len -> len
    | None -> List.fold_right (( + ) <.> String.length) payloads 0 in
  Allocate (kind, payloads, len)

let delete addr len = Delete (addr, len)

let collect addr len uid = Collect (addr, len, uid)
*)

let _prefix = 4
let _header_prefix = 0
let _header_prefix_count = _header_prefix + _prefix
(* XXX(dinosaure): [prefix_count] is **not** the length of [prefix]
   if [prefix_count > _prefix]. In that case, we do a compression-path
   of [prefix_count] bytes and save only 4 bytes of this prefix into
   [prefix] to help a pessimistic check. *)

let _header_kind = _header_prefix_count + 4
let _header_owner =
  if Sys.word_size = 64 then _header_kind + 8 else _header_kind + 4
(* XXX(dinosaure): [owner] is a unique identifier (like a PID) to tell us which
   process requires a node to be alive. It helps us to collect and re-use nodes
   which are not needed anymore by any readers.

   To ensure that the node can be re-used, we use this unique ID and keep
   globally who wants to read to the tree (readers) (into a [ring]). If this ID
   does not exist anymore inside our [ring], we can safely re-use the node. *)
let _header_depth =
  if Sys.word_size = 64 then _header_owner + 8 else _header_owner + 4
(* XXX(dinosaure): [depth] includes [prefix]. For example, we have a
   path-compression of 4-bytes into our **second**-level node (and parent of it
   has no prefix):

   depth = parent(depth) + length(prefix) + 1 = 5

   [depth] is a **constant**, it permits to set the prefix safely while reading.
   [find] trusts on this value first before to look into
   [prefix]/[prefix_count].
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

(* XXX(dinosaure): note for me, [msync(2)] does not ensure the **order** of
   writes and I'm really not sure about the use of it and [clflush] which
   flushes the memory [_cache_line_size] by [_cache_line_size]. By this way, we
   ensure that even if we don't control the order of writes on these areas, it
   seems that P-ART (see RECIPE paper) ensures orders in the design of ROWEX.

   The conversion action from ROWEX to P-ART is: Insert cache line flush
   and memory fence instructions after **each** [store].

   I got more informations, if we use [mmap] which is the case here (see
   [persistent.ml]), we ask to the kernel to map a file into memory and then
   expose this memory region into the application's virtual address space.

   Such region is treated as byte-addressable storage, Behind the scenes, page
   caching occurs, which is where the kernel pauses the application to perform
   the I/O operation, but the underlying storage can only talk in blocks
   ([pagesize]). So, even if a single byte is changed, the entire 4K block is
   moved to storage, which is not very efficient.

   So we must ensure that when we write something, we **really** write
   something to ensure the "power-fail" atomicity. [clflush] with [fence]
   help us about that when it ensure our write in a failure protected domain.
   [clflushopt] is **weakly** ordered (as [msync]) so we must follow it by
   an [sfence] instruction. *)

module Make (S : S) = struct
  let ( let* ) = S.bind
  let ( >>| ) x f = S.bind x (S.return <.> f)

  open S

  let _pause etsc =
    let* ctsc = rdtsc in
    if ctsc < etsc then pause_intrinsic
    else return ()

  let rec _clflush addr len =
    let* etsc = rdtsc in
    let etsc = etsc + (_write_latency_in_ns * _cpu_freq_mhz / 1000) in
    let* () = clflush addr in
    let* () = _pause etsc in
    if len - _cache_line_size > 0
    then _clflush Addr.(addr + _cache_line_size) (len - _cache_line_size)
    else return ()

  let clflush (addr : [ `Rd | `Wr ] Addr.t) len front back =
    let addr = (addr :> int) land (lnot (_cache_line_size - 1)) in
    let* () = if front then sfence else return () in
    let* () = _clflush (Addr.of_int_wronly addr) len in
    let* () = if back then sfence else return () in
    return ()

  let movnt64 dst value front back =
    let* () = if front then sfence else return () in
    let* () = stream_int dst value in
    let* () = if back then sfence else return () in
    return ()

(* XXX(dinosaure): impossible by the type-system. *)
(* let _ = atomic_get ~memory_order:Release (Addr.of_int_rdonly 0) Value.int8
   [Release] is only used to [store]/[set]. *)
  (* let _ = atomic_set (Addr.of_int_rdonly 0) Value.int8 0
     Impossible to set a read-only value. *)
  (* let _ = atomic_get (Addr.of_int_wronly 0) Value.int8
     Impossible to get a write-only value. *)
  (* let _ = atomic_get (Addr.of_int_rdonly 0) Value.(string 10)
     Impossible to load atomically a non-atomic value *)

  let get_version addr =
    atomic_get Addr.(addr + _header_kind) Value.leintnat [@@inline]

  let get_type addr =
    let* value = atomic_get ~memory_order:Relaxed Addr.(addr + _header_kind)
        Value.leintnat in
    return (value lsr _bits_kind)
  [@@inline]

  let get_prefix (addr : [> `Rd ] Addr.t) =
    (* XXX(dinosaure): may be we can optimize this part with [Value.leintnat]
       for a 64-bits architecture. However, we assume that 1 bit will disappear.
       Considering little-endian architecture, we probably should start with
       [prefix_count] and, then, [prefix]. [prefix_count] can ~safely~ fit into
       31 bits and [prefix] will use the rest (32 bits).

       By this way, we permit to use a native integer instead a boxed [int64].

       TODO!

       So we consider that the only layout possible of values such as [leint64]
       is a little-endian layout (no way!). I did a mistake about a possible
       abstraction over /endian/ but it's much more simpler to consider all with
       little-endian. By this way, the assumption between [prefix_count] and
       [prefix] remains and it's a good news. However, we should check that! *)
    let* value = atomic_get Addr.(addr + _header_prefix) Value.leint64 in
    let p0 = Int64.(to_int (logand value 0xffffL)) in
    let p1 = Int64.(to_int (logand (shift_right value 16) 0xffffL)) in
    let prefix = Bytes.create _prefix in
    bytes_set16 prefix 0 p0 ;
    bytes_set16 prefix 2 p1 ;
    return (Bytes.unsafe_to_string prefix,
            Int64.(to_int (shift_right value 32)))

  let ( >>= ) x f = bind x f

  let set_prefix addr ~prefix ~prefix_count flush =
    if prefix_count = 0
    then atomic_set ~memory_order:Release Addr.(addr + _header_prefix)
        Value.leint64 0L
    else
      let p0 = string_get16 prefix 0 in
      let p1 = string_get16 prefix 2 in
      let prefix = Int64.(logor (shift_left (of_int p1) 16) (of_int p0)) in
      let rs = Int64.(logor (shift_left (of_int prefix_count) 32) prefix) in
      atomic_set ~memory_order:Release Addr.(addr + _header_prefix)
        Value.leint64 rs >>= fun () ->
    if flush
    then clflush Addr.(addr + _header_prefix) 8 false true
    else return ()

  (**** FIND CHILD ****)

  let n4_find_child addr k =
    let* _0 = atomic_get Addr.(addr + _header_length + 0) Value.int8 in
    if _0 = k
    then atomic_get Addr.(addr + _header_length + 4 + (Addr.length * 0))
        Value.addr_rd else
    let* _1 = atomic_get Addr.(addr + _header_length + 1) Value.int8 in
    if _1 = k
    then atomic_get Addr.(addr + _header_length + 4 + (Addr.length * 1))
        Value.addr_rd else
    let* _2 = atomic_get Addr.(addr + _header_length + 2) Value.int8 in
    if _2 = k
    then atomic_get Addr.(addr + _header_length + 4 + (Addr.length * 2))
        Value.addr_rd else
    let* _3 = atomic_get Addr.(addr + _header_length + 3) Value.int8 in
    if _3 = k
    then atomic_get Addr.(addr + _header_length + 4 + (Addr.length * 3))
        Value.addr_rd else
      ( Log.debug (fun m -> m "No child for %02x into N4" k)
      ; return Addr.null )

  external n16_get_child : int -> int -> string -> int = "caml_n16_get_child"
    [@@noalloc]
  external ctz : int -> int = "caml_ctz" [@@noalloc]

  (* XXX(dinosaure): despite [art.ml], [N4] and [N16] aren't order.
     We must check all children. *)

  let rec _n16_find_child addr k bitfield =
    if bitfield = 0 then return Addr.null
    else
      let p = ctz bitfield in
      let* k' = atomic_get Addr.(addr + _header_length + p) Value.int8 in
      let* value = atomic_get
          Addr.(addr + _header_length + 16 + (p * Addr.length)) Value.addr_rd in
      if not (Addr.is_null value) && k' = (k lxor 128)
      then return value
      else _n16_find_child addr k (bitfield lxor (1 lsl p))

  let n16_find_child addr k =
    let* keys = atomic_get Addr.(addr + _header_length) Value.leint128 in
    (* XXX(dinosaure): Dragoon here! How to load atomically a 128 bits integer
       and save it into a string? *)
    let bitfield = n16_get_child 16 k keys in
    _n16_find_child addr k bitfield

  let n48_find_child addr k =
    let* pos' = atomic_get Addr.(addr + _header_length + k) Value.int8 in
    if pos' <> 48
    then atomic_get Addr.(addr + _header_length + 256 + (Addr.length * pos'))
        Value.addr_rd
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
      let* child' : [ `Rd ] Addr.t = atomic_get
          Addr.(addr + _header_length + header + (idx * Addr.length))
          Value.addr_rd in
      if (child' :> int) land 1 = 1 then return child'
      else _node_any_child addr ~header (if not (Addr.is_null child')
                                         then child'
                                         else child)
          (succ idx) max
  [@@inline]

  let n4_any_child addr   = _node_any_child addr ~header:4   Addr.null 0 4
  let n16_any_child addr  = _node_any_child addr ~header:16  Addr.null 0 16
  let n48_any_child addr  = _node_any_child addr ~header:256 Addr.null 0 48
  let n256_any_child addr = _node_any_child addr ~header:0   Addr.null 0 256

  (* XXX(dinosaure): thx @Drup. *)
  type formatter =
    { commit : unit -> unit S.t
    ; ppf : Format.formatter }

  let formatter ~commit ppf = { commit; ppf; }

  let kfprintf
    : (formatter -> unit S.t -> 'a) -> formatter ->
      ('b, Format.formatter, unit, 'a) format4 -> 'b
    = fun k ppft fmt -> Format.kfprintf (fun _ppf -> k ppft @@ ppft.commit ())
        ppft.ppf fmt

  let fprintf
    : formatter -> ('a, Format.formatter, unit, unit S.t) format4 -> 'a
    = fun ppft fmt -> kfprintf (fun _ t -> t) ppft fmt

  let[@coverage off] pp_char ppf = function
    | '\x21' .. '\x7e' as chr -> Fmt.char ppf chr
    | chr -> Fmt.pf ppf "%02x" (Char.code chr)

  let pp_n4 ppf addr =
    let* _0 = atomic_get Addr.(addr + _header_length + 0) Value.int8 in
    let* _1 = atomic_get Addr.(addr + _header_length + 1) Value.int8 in
    let* _2 = atomic_get Addr.(addr + _header_length + 2) Value.int8 in
    let* _3 = atomic_get Addr.(addr + _header_length + 3) Value.int8 in
    let arr = [| _0; _1; _2; _3 |] in
    fprintf ppf "%a" Fmt.(Dump.array (using Char.unsafe_chr pp_char)) arr

  let pp_n16 ppf addr =
    let* ks = atomic_get Addr.(addr + _header_length) Value.leint128 in
    let arr = Array.init 16 (fun i -> ks.[i]) in
    fprintf ppf "%a" Fmt.(Dump.array pp_char) arr

  let pp_n48 ppf addr =
    let rec go arr i =
      if i = 48 then return arr
      else let* chr = atomic_get Addr.(addr + _header_length + i) Value.int8 in
        ( arr.(i) <- Char.unsafe_chr chr ; go arr (succ i) ) in
    let* arr = go (Array.make 48 '\000') 0 in
    fprintf ppf "%a" Fmt.(Dump.array pp_char) arr

  let pp_n256 ppf _addr = fprintf ppf "n256"

  let pp_keys ppf addr =
    let* ty = get_type addr in
    match ty with
    | 0 -> pp_n4 ppf addr
    | 1 -> pp_n16 ppf addr
    | 2 -> pp_n48 ppf addr
    | 3 -> pp_n256 ppf addr
    | _ -> assert false

  let pp_kind ppf addr =
    let* ty = get_type addr in
    match ty with
    | 0 -> fprintf ppf "%a" Fmt.string "N4"
    | 1 -> fprintf ppf "%a" Fmt.string "N16"
    | 2 -> fprintf ppf "%a" Fmt.string "N48"
    | 3 -> fprintf ppf "%a" Fmt.string "N256"
    | _ -> assert false

  let pp_record ppf addr =
    let* prefix, prefix_count = get_prefix addr in
    let* depth = get Addr.(addr + _header_depth) Value.leint31 in
    let* () = fprintf ppf
        "{ @[<hov>prefix= %S;@ prefix_count= %d;@ depth= %d;@ kind= "
        prefix prefix_count depth in
    let* () = pp_kind ppf addr in
    let* () = fprintf ppf ";@] }" in
    return ()

  let rec pp_children ~header ~n:max ppf addr =
    let rec go ~header idx arr =
      if idx < max
      then
        let addr = Addr.(addr + _header_length + header + (idx * Addr.length)) in
        let* addr = atomic_get addr Value.addr_rd in
        ( arr.(idx) <- addr ; go ~header (succ idx) arr )
      else return arr in
    let* arr = go ~header 0 (Array.init max (fun _ -> Addr.null)) in
    let rec pp ppf = function
      | [] -> return ()
      | [ x ] -> pp_elt ppf x
      | x :: r ->
        let* () = pp_elt ppf x in
        let* () = fprintf ppf ";@ " in
        pp ppf r in
    let* () = fprintf ppf "[|@[<hov>" in
    let* () = pp ppf (Array.to_list arr) in
    let* () = fprintf ppf "@]|]" in
    return ()

  and pp_elt ppf (addr : [ `Rd ] Addr.t) =
    if Addr.is_null addr then fprintf ppf "<null>"
    else if (addr :> int) land 1 = 1
    then
      let leaf = Leaf.prj (Addr.unsafe_to_leaf addr) in
      let* key = get leaf Value.c_string in
      let len = (String.length key + size_of_word) / size_of_word in
      (* padding *)
      let len = len * size_of_word in
      let* value = get Addr.(leaf + len) Value.leintnat in
      fprintf ppf "{:leaf @[<hov>key= %S;@ value= %d;@] }" key value
    else
      let* n, header = get_type addr >>| function
        | 0 -> 4, 4 | 1 -> 16, 16 | 2 -> 48, 256 | _ -> 256, 0 in
      let* () = fprintf ppf "{:node @[<hov>hdr= @[<hov>" in
      let* () = pp_record ppf addr in
      let* () = fprintf ppf "@];@ key= @[<hov>" in
      let* () = pp_keys ppf addr in
      let* () = fprintf ppf "@];@ children= @[<hov>" in
      let* () = pp_children ~header ~n ppf addr in
      let* () = fprintf ppf "@];@] }" in
      return ()

  let pp ppf (root : [> `Rd ] Addr.t) =
    pp_elt ppf (Addr.to_rdonly root)

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

  let minimum (addr : [> `Rd ] Addr.t) = minimum (Addr.to_rdonly addr)
  [@@inline]

  let find_child (addr : [> `Rd ] Addr.t) chr =
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

  let check_prefix (addr : [> `Rd ] Addr.t) ~key ~key_len level =
    let* depth = get Addr.(addr + _header_depth) Value.leint31 in
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
        then return (max + (prefix_count - _prefix))
        (* XXX(dinosaure): optimistic match *)
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

  let rec _check_prefix_pessimistic
      ~key ~minimum ~prefix ~prefix_count ~level idx max =
    Log.debug (fun m -> m "check_prefix_pessimistic idx:%d, max:%d" idx max) ;
    if idx = max then return (Match { level= level land 0x7fffffff })
    else
      let* chr =
        (* XXX(dinosaure): note that a minimum, a leaf is, in ANYWAY,
           a constant value. It's why we can keep it as a [addr Lazy.t]
           without any trouble. *)
        if idx >= _prefix
        then Lazy.force minimum >>| fun key' ->
          Log.debug (fun m -> m "Get the byte %d from the minimum %S." (level land 0x7fffffff) key') ;
          key'.![level land 0x7fffffff]
        else return prefix.[idx] in
      Log.debug (fun m -> m "(prefix | minimum(node).key)[%d]:%02x <> key.[%d]:%02x" idx (Char.code chr) (level land 0x7fffffff)
        (Char.code key.![level land 0x7fffffff])) ;
      if chr <> key.![level land 0x7fffffff]
      then
        let non_matching_key = chr in
        let* non_matching_prefix =
          if prefix_count > _prefix
          then
            let res = Bytes.make _prefix '\000' in
            let* key = Lazy.force minimum in
            let len = min
                (prefix_count - ((level land 0x7fffffff) - (level lsr 31)) - 1)
                _prefix in
            Log.debug (fun m -> m "len:%d > 0 then blit %S %d bytes 0 %d" len key ((level land 0x7fffffff) + 1) len) ;
            if len > 0
            then Bytes.blit_string key ((level land 0x7fffffff) + 1) res 0 len ;
            return (Bytes.unsafe_to_string res)
          else
            let res = Bytes.make _prefix '\000' in
            Log.debug (fun m -> m "blit %S (idx:%d + 1) res 0 (prefix_count:%d - idx:%d - 1)" prefix idx prefix_count idx) ;
            if prefix_count - idx - 1 > 0
            then Bytes.blit_string prefix (idx + 1) res 0
                (prefix_count - idx - 1) ;
            return (Bytes.unsafe_to_string res) in
        return (No_match { non_matching_key
                         ; non_matching_prefix
                         ; level= level land 0x7fffffff })
      else _check_prefix_pessimistic
          ~key ~minimum ~prefix ~prefix_count ~level:(succ level) (succ idx) max

  let check_prefix_pessimistic (addr : [> `Rd ] Addr.t) ~key level =
    let* prefix, prefix_count = get_prefix addr in
    Log.debug (fun m -> m "prefix: %S, prefix-count: %d." prefix prefix_count) ;
    let* depth = get Addr.(addr + _header_depth) Value.leint31 in
    if prefix_count + level < depth
    then return Skipped_level
    else if prefix_count > 0
    then
      let idx = (level + prefix_count) - depth in
      let max = prefix_count in
      let minimum = Lazy.from_fun @@ fun () ->
        let* leaf = minimum addr in
        get (Leaf.prj (Addr.unsafe_to_leaf leaf)) Value.c_string in
      _check_prefix_pessimistic ~key ~minimum
        ~prefix ~prefix_count
        ~level:((level lsl 31) lor level) idx max
    else return (Match { level })
        (* XXX(dinosaure): even if [level] still is the same,
           it seems that [_check_prefix_pessimistic] can return with
           an other [level] value. *)

  let rec _lookup
      (node : [> `Rd ] Addr.t) ~key ~key_len ~optimistic_match level =
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
             let len = (String.length key + size_of_word) / size_of_word in
             (* padding *)
             let len = len * size_of_word in
             get Addr.(leaf + len) Value.leintnat
           else
             let len = (String.length key + size_of_word) / size_of_word in
             (* padding *)
             let len = len * size_of_word in
             get Addr.(leaf + len) Value.leintnat )
    else _lookup node ~key ~key_len ~optimistic_match (succ level)

  let lookup
    : [> `Rd ] Addr.t -> key:string -> key_len:int -> int t
    = fun (node : ([> `Rd ] as 'a) Addr.t) ~key ~key_len ->
    let node = Addr.unsafe_to_int node in
    let node = Addr.of_int_rdonly node in
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

  let _sizeof_n4 = _header_length + 4 + (4 * Addr.length)
  let _sizeof_n16 = _header_length + 16 + (16 * Addr.length)
  let _sizeof_n48 = _header_length + 256 + (48 * Addr.length)
  let _sizeof_n256 = _header_length + (256 * Addr.length)

  (***** ADD CHILD *****)

  (* XXX(dinosaure): a note about the diff between ROWEX and P-ART. [add_child]
     has a new argument where we ensure to flush the cache or not - indeed, in
     the paper, we can have the opportunity to "safely" trust on the cache and
     avoid a real write (and bypass the cache).

     However, it appears that in some situation, we need to execute [clflush]
     and [movnt64] to ensure a synchro between readers and writers. *)

  let add_child_n256 (N256 addr) k (value : [ `Rd ] Addr.t) flush =
    if flush
    then
      let* () = movnt64
          Addr.(to_wronly (addr + _header_length + (k * Addr.length)))
          (value :> int) false true in
      let* _  = fetch_add
          Addr.(addr + _header_count)
          Value.leint16 1 in
      return true
    else
      let* () = atomic_set ~memory_order:Relaxed
          Addr.(addr + _header_length + (k * Addr.length))
          Value.addr_rd value in
      let* _  = fetch_add
          Addr.(addr + _header_count)
          Value.leint16 1 in
      return true

  (* TODO(dinosaure): [P-ART] is different from [ROWEX]. *)
  let add_child_n48 (N48 addr) k value _flush =
    let* compact_count =
      atomic_get Addr.(addr + _header_compact_count) Value.leint16 in
    if compact_count = 48
    then return false
    else
      let* () = atomic_set ~memory_order:Release
          Addr.(addr + _header_length + 256 + (compact_count * Addr.length))
          Value.addr_rd value in
      let* () = atomic_set ~memory_order:Release
          Addr.(addr + _header_length + k)
          Value.int8 compact_count in
      let* _  = fetch_add
          Addr.(addr + _header_compact_count)
          Value.leint16 1 in
      let* _  = fetch_add
          Addr.(addr + _header_count)
          Value.leint16 1 in
      return true

  let add_child_n16 (N16 addr) k (value : [ `Rd ] Addr.t) flush =
    let* compact_count =
      atomic_get Addr.(addr + _header_compact_count) Value.leint16 in
    if compact_count = 16
    then return false
    else
      let* _next_index = fetch_add
          Addr.(addr + _header_compact_count)
          Value.leint16 1 in
      let* _          = fetch_add
          Addr.(addr + _header_count)
          Value.leint16 1 in
      if flush
      then
        let* () = atomic_set ~memory_order:Release
            Addr.(addr + _header_length + compact_count)
            Value.int8 (k lxor 128) in
        let* () = clflush addr Addr.length false true in
        let* () = movnt64
            Addr.(to_wronly
                    (addr + _header_length + 16
                     + (compact_count * Addr.length)))
            (value :> int) false true in
        return true
      else
        let* () = atomic_set ~memory_order:Relaxed
            Addr.(addr + _header_length + compact_count)
            Value.int8 (k lxor 128) in
        let* () = atomic_set ~memory_order:Relaxed
            Addr.(addr + _header_length + 16 + (compact_count * Addr.length))
            Value.addr_rd value in
        return true

  let add_child_n4 (N4 addr) k (value : [ `Rd ] Addr.t) flush =
    let* compact_count =
      atomic_get Addr.(addr + _header_compact_count) Value.leint16 in
    if compact_count = 4
    then return false
    else
      let* _next_index = fetch_add
          Addr.(addr + _header_compact_count)
          Value.leint16 1 in
      let* _          = fetch_add
          Addr.(addr + _header_count)
          Value.leint16 1 in
      if flush
      then
        let* () = atomic_set ~memory_order:Release
            Addr.(addr + _header_length + compact_count)
            Value.int8 k in
        let* () = clflush addr _sizeof_n4 false true in
        let* () = movnt64
            Addr.(to_wronly (addr + _header_length + 4
                             + (compact_count * Addr.length)))
            (value :> int) false true in
        return true
      else
        let* () = atomic_set ~memory_order:Relaxed
            Addr.(addr + _header_length + compact_count)
            Value.int8 k in
        let* () = atomic_set ~memory_order:Relaxed
            Addr.(addr + _header_length + 4 + (compact_count * Addr.length))
            Value.addr_rd value in
        return true

  let write_unlock addr =
    let* _ = fetch_add Addr.(addr + _header_kind) Value.leintnat 0b10 in
    return ()

  let write_unlock_and_obsolete addr =
    let* _ = fetch_add Addr.(addr + _header_kind) Value.leintnat 0b11 in
    return ()

  let is_obsolete version = (version land 1 = 1)

  let _read_unlock_or_restart addr expected =
    let* value = atomic_get Addr.(addr + _header_kind) Value.leintnat in
    return (expected = value)
  [@@inline]

  (* XXX(dinosaure): spin-lock *)

  let rec until_is_locked addr version =
    if version land 0b10 = 0b10
    then
      let* () = pause_intrinsic in
      let* version = atomic_get Addr.(addr + _header_kind) Value.leintnat in
      until_is_locked addr version
    else return version
  [@@inline]

  let rec write_lock_or_restart addr need_to_restart =
    let* version = atomic_get Addr.(addr + _header_kind) Value.leintnat in
    let* version = until_is_locked addr version in
    if is_obsolete version
    then ( need_to_restart := true ; return () )
    else
      let* res = compare_exchange ~weak:true Addr.(addr + _header_kind)
          Value.leintnat (ref version) (version + 0b10) in
      if not res then write_lock_or_restart addr need_to_restart else return ()
  [@@inline]

  let lock_version_or_restart addr version need_to_restart =
    if (version land 0b10 = 0b10)|| (version land 1 = 1)
    then ( need_to_restart := true ; return version)
    else
      let* set = compare_exchange Addr.(addr + _header_kind)
          Value.leintnat (ref version) (version + 0b10) in
      if set then return (version + 0b10)
      else ( need_to_restart := true ; return version )

  (***** CHANGE/UPDATE CHILD *****)

  (* XXX(dinosaure): may be do an optimisation pass on this part of the code.
     For example, in [_n4_update_child], [compact_count] should be loaded only
     one time. The check of [child] to see if it's not [NULL] can be deleted
     if our assumptions are rights.

     Should we protect [addr] with typed constructor? *)

  let rec _n4_update_child addr k ptr i =
    let* compact_count = atomic_get Addr.(addr + _header_compact_count)
        Value.leint16 in
    if i < compact_count
    then
      let* key = atomic_get Addr.(addr + _header_length + i) Value.int8 in
      let* child = atomic_get
          Addr.(addr + _header_length + 4 + (i * Addr.length)) Value.addr_rd in
      if not (Addr.is_null child) && key = k
      then atomic_set Addr.(addr + _header_length + 4 + (i * Addr.length))
          Value.addr_rd ptr
      else _n4_update_child addr k ptr (succ i)
    else assert false (* XXX(dinosaure): impossible or integrity problem! *)

  let n4_update_child addr k ptr = _n4_update_child addr k ptr 0

  let rec _n16_child_pos addr k bitfield =
    if bitfield = 0 then assert false
    else
      let p = ctz bitfield in
      let* k' = atomic_get Addr.(addr + _header_length + p) Value.int8 in
      let* value = atomic_get
          Addr.(addr + _header_length + 16 + (p * Addr.length)) Value.addr_rd in
      if not (Addr.is_null value) && k' = (k lxor 128)
      then return Addr.(to_rdonly
                          (addr + _header_length + 16 + (p * Addr.length)))
      else _n16_child_pos addr k (bitfield lxor (1 lsl p))

  let _n16_child_pos addr k =
    let* compact_count =
      atomic_get Addr.(addr + _header_compact_count) Value.leint16 in
    let* keys = atomic_get Addr.(addr + _header_length) Value.leint128 in
    let bitfield = n16_get_child compact_count k keys in
    _n16_child_pos addr k bitfield

  let n16_update_child addr k ptr =
    let* addr = _n16_child_pos addr k in
    let addr = Addr.of_int_wronly (addr :> int) in (* XXX(dinosaure): unsafe! *)
    atomic_set ~memory_order:Release addr Value.addr_rd ptr

  let n48_update_child addr k ptr =
    let* idx = atomic_get Addr.(addr + _header_length + k) Value.int8 in
    atomic_set ~memory_order:Release
      Addr.(addr + _header_length + 256 + (idx * Addr.length)) Value.addr_rd ptr

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

  let _count = String.make 2 '\000'
  let _compact_count = String.make 2 '\000'

  let _n4_ks = String.make 4 '\000'
  let _n4_vs = String.concat "" (List.init 4 (const string_of_null_addr))

  let alloc_n4 ~prefix:p ~prefix_count ~level =
    let prefix = Bytes.make 4 '\000' in
    Bytes.blit_string p 0 prefix 0 (min _prefix (String.length p)) ;
    let prefix_count = leint31_to_string prefix_count in
    let k = leintnat_to_string ((_n4_kind lsl _bits_kind) lor 0b100) in
    let o = leintnat_to_string 0 in
    let l = leint31_to_string level in
    allocate ~kind:`Node
      [ Bytes.unsafe_to_string prefix; prefix_count; k; o; l; _count
      ; _compact_count; _n4_ks; _n4_vs ]
      ~len:_sizeof_n4 >>| n4

  let _n16_ks = String.make 16 '\000'
  let _n16_vs = String.concat "" (List.init 16 (const string_of_null_addr))

  let alloc_n16 ~prefix:p ~prefix_count ~level =
    let prefix = Bytes.make 4 '\000' in
    Bytes.blit_string p 0 prefix 0 (min _prefix (String.length p)) ;
    let prefix_count = leint31_to_string prefix_count in
    let k = leintnat_to_string ((_n16_kind lsl _bits_kind) lor 0b100) in
    let o = leintnat_to_string 0 in
    let l = leint31_to_string level in
    allocate ~kind:`Node
      [ Bytes.unsafe_to_string prefix; prefix_count; k; o; l; _count
      ; _compact_count; _n16_ks; _n16_vs ]
      ~len:_sizeof_n16 >>| n16

  let _n48_ks = String.make 256 '\048'
  let _n48_vs = String.concat "" (List.init 48 (const string_of_null_addr))

  let alloc_n48 ~prefix:p ~prefix_count ~level =
    let prefix = Bytes.make 4 '\000' in
    Bytes.blit_string p 0 prefix 0 (min _prefix (String.length p)) ;
    let prefix_count = leint31_to_string prefix_count in
    let k = leintnat_to_string ((_n48_kind lsl _bits_kind) lor 0b100) in
    let o = leintnat_to_string 0 in
    let l = leint31_to_string level in
    allocate ~kind:`Node
      [ Bytes.unsafe_to_string prefix; prefix_count; k; o; l; _count
      ; _compact_count; _n48_ks; _n48_vs ]
      ~len:_sizeof_n48 >>| n48

  let _n256_vs = String.concat "" (List.init 256 (const string_of_null_addr))

  let alloc_n256 ~prefix:p ~prefix_count ~level =
    let prefix = Bytes.make 4 '\000' in
    Bytes.blit_string p 0 prefix 0 (min _prefix (String.length p)) ;
    let prefix_count = leint31_to_string prefix_count in
    let k = leintnat_to_string ((_n256_kind lsl _bits_kind) lor 0b100) in
    let o = leintnat_to_string 0 in
    let l = leint31_to_string level in
    allocate ~kind:`Node
      [ Bytes.unsafe_to_string prefix; prefix_count; k; o; l; _count
      ; _compact_count; _n256_vs ]
      ~len:_sizeof_n256 >>| n256

  (***** COPY CHILD <N0, N1> (assert (sizeof(N0) <= sizeof(N1))) *****)

  let rec _copy_n4_into_n16 ~compact_count n4 n16 i =
    if i = compact_count
    then return ()
    else
      let* value = atomic_get Addr.(n4 + _header_length + 4 + (i * Addr.length))
          Value.addr_rd in
      match Addr.is_null value with
      | true  -> _copy_n4_into_n16 ~compact_count n4 n16 (succ i)
      | false ->
        let* key = atomic_get Addr.(n4 + _header_length + i) Value.int8 in
        let* _   = add_child_n16 n16 key value false in
        (* XXX(dinosaure): assert (_ = true); *)
        Log.debug (fun m -> m "<N4 -> N16> copy %02x (%016x)"
                      key (value :> int)) ;
        _copy_n4_into_n16 ~compact_count n4 n16 (succ i)

  let copy_n4_into_n16 (N4 n4) n16 =
    let* compact_count =
      atomic_get Addr.(n4 + _header_compact_count) Value.leint16 in
    _copy_n4_into_n16 ~compact_count n4 n16 0

  (* XXX(dinosaure): [copy_n4_into_n4] is called when:
     - [compact_count = 4]
     - [count <= 3]

     Such case appears about deletion when we decrease only [count]. So we must
     scan any objects & copy them into the new node if they are not equal to
     [null]. *)

  let rec _copy_n4_into_n4 nx ny i =
    if i = 4
    then return ()
    else
      let* value = atomic_get Addr.(nx + _header_length + 4 + (i * Addr.length))
          Value.addr_rd in
      match Addr.is_null value with
      | true  -> _copy_n4_into_n4 nx ny (succ i)
      | false ->
        let* key = atomic_get Addr.(nx + _header_length + i) Value.int8 in
        let* _   = add_child_n4 ny key value false in
        (* XXX(dinosaure): assert (_ = true); *)
        _copy_n4_into_n4 nx ny (succ i)

  let copy_n4_into_n4 (N4 nx) ny = _copy_n4_into_n4 nx ny 0

  let rec _copy_n16_into_n48 ~compact_count n16 n48 i =
    if i = compact_count
    then return ()
    else
      let* value = atomic_get
          Addr.(n16 + _header_length + 16 + (i * Addr.length)) Value.addr_rd in
      match Addr.is_null value with
      | true  -> _copy_n16_into_n48 ~compact_count n16 n48 (succ i)
      | false ->
        let* key = atomic_get Addr.(n16 + _header_length + i) Value.int8 in
        let* _   = add_child_n48 n48 (key lxor 128) value false in
        (* XXX(dinosaure): assert (_ = true); *)
        _copy_n16_into_n48 ~compact_count n16 n48 (succ i)

  let copy_n16_into_n48 (N16 n16) n48 =
    let* compact_count = atomic_get Addr.(n16 + _header_compact_count)
        Value.leint16 in
    _copy_n16_into_n48 ~compact_count n16 n48 0

  let rec _copy_n16_into_n16 nx ny i =
    if i = 16
    then return ()
    else
      let* value = atomic_get
          Addr.(nx + _header_length + 16 + (i * Addr.length)) Value.addr_rd in
      match Addr.is_null value with
      | true  -> _copy_n16_into_n16 nx ny (succ i)
      | false ->
        let* key = atomic_get Addr.(nx + _header_length + i) Value.int8 in
        let* _   = add_child_n16 ny (key lxor 128) value false in
        (* XXX(dinosaure): ssert (_ = true); *)
        _copy_n16_into_n16 nx ny (succ i)

  let copy_n16_into_n16 (N16 nx) ny = _copy_n16_into_n16 nx ny 0

  let rec _copy_n48_into_n256 n48 n256 k =
    if k = 256 then return ()
    else
      let* index = atomic_get Addr.(n48 + _header_length + k) Value.int8 in
      match index with
      | 48  -> _copy_n48_into_n256 n48 n256 (succ k)
      | _ ->
        let* value = atomic_get
            Addr.(n48 + _header_length + 256 + (index * Addr.length))
            Value.addr_rd in
        let* _ = add_child_n256 n256 k value false in
        (* XXX(dinosaure): assert (_ = true); *)
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
        let* value = atomic_get
            Addr.(nx + _header_length + 256 + (index * Addr.length))
            Value.addr_rd in
        let* _ = add_child_n48 ny k value false in
        (* XXX(dinosaure): assert (_ = true); *)
        _copy_n48_into_n48 nx ny (succ k)

  let copy_n48_into_n48 (N48 nx) ny = _copy_n48_into_n48 nx ny 0

  let _insert_grow_n4_n16 (N4 addr as n4) p k kp value need_to_restart =
    let* inserted = add_child_n4 n4 k value true in
    if inserted then write_unlock addr
    else
      ( Log.debug (fun m -> m "We must grow the N4 node to a N16 node")
      ; let* prefix, prefix_count = get_prefix addr in
        let* level = get Addr.(addr + _header_depth) Value.leint31 in
        let* N16 addr' as n16 = alloc_n16 ~prefix ~prefix_count ~level in
        Log.debug (fun m -> m "Copy N4 children into new N16 node")
      ; let* () = copy_n4_into_n16 n4 n16 in
        let* _  = add_child_n16 n16 k value false in
        (* XXX(dinosaure): assert (_ = true); *)
        let* () = write_lock_or_restart p need_to_restart in
        if !need_to_restart
        then ( let* () = delete addr'
                   (_header_length + 16 + (Addr.length * 16)) in
               write_unlock addr )
        else
          let* () = clflush addr' _sizeof_n16 false true in
          let* () = update_child p kp (Addr.to_rdonly addr') in
          let* () = write_unlock p in
          let* () = write_unlock_and_obsolete addr in
          let* uid = atomic_get Addr.(addr + _header_owner) Value.leintnat in
          collect addr ~len:(_header_length + 4 + (Addr.length * 4)) ~uid )

  let _insert_grow_n16_n48 (N16 addr as n16) p k kp value need_to_restart =
    let* inserted = add_child_n16 n16 k value true in
    if inserted then write_unlock addr
    else
      let* prefix, prefix_count = get_prefix addr in
      let* level = get Addr.(addr + _header_depth) Value.leint31 in
      let* N48 addr' as n48 = alloc_n48 ~prefix ~prefix_count ~level in
      let* () = copy_n16_into_n48 n16 n48 in
      let* _  = add_child_n48 n48 k value false in
      (* XXX(dinosaure): assert (_ = true); *)
      let* () = write_lock_or_restart p need_to_restart in
      if !need_to_restart
      then ( let* () = delete addr'
                 (_header_length + 256 + (Addr.length * 48)) in
             write_unlock addr )
      else
        let* () = clflush addr' _sizeof_n48 false true in
        let* () = update_child p kp (Addr.to_rdonly addr') in
        let* () = write_unlock p in
        let* () = write_unlock_and_obsolete addr in
        let* uid = atomic_get Addr.(addr + _header_owner) Value.leintnat in
        collect addr ~len:(_header_length + 16 + (Addr.length * 16)) ~uid

  let _insert_grow_n48_n256 (N48 addr as n48) p k kp value need_to_restart =
    let* inserted = add_child_n48 n48 k value true in
    if inserted then write_unlock addr
    else
      let* prefix, prefix_count = get_prefix addr in
      let* level = get Addr.(addr + _header_depth) Value.leint31 in
      let* N256 addr' as n256 = alloc_n256 ~prefix ~prefix_count ~level in
      let* () = copy_n48_into_n256 n48 n256 in
      let* _  = add_child_n256 n256 k value false in
      (* XXX(dinosaure): assert (_ = true); *)
      let* () = write_lock_or_restart p need_to_restart in
      if !need_to_restart
      then ( let* () = delete addr'
                 (_header_length + 256 + (Addr.length * 256)) in
             write_unlock addr )
      else
        let* () = clflush addr' _sizeof_n256 false true in
        let* () = update_child p kp (Addr.to_rdonly addr') in
        let* () = write_unlock p in
        let* () = write_unlock_and_obsolete addr in
        let* uid = atomic_get Addr.(addr + _header_owner) Value.leintnat in
        collect addr ~len:(_header_length + 256 + (Addr.length * 48)) ~uid

  let insert_compact_n4 (N4 addr as n4) p k kp value need_to_restart =
    let* prefix, prefix_count = get_prefix addr in
    let* level = get Addr.(addr + _header_depth) Value.leint31 in
    let* N4 addr' as n4' = alloc_n4 ~prefix ~prefix_count ~level in
    let* () = copy_n4_into_n4 n4 n4' in
    let* _  = add_child_n4 n4' k value false in
    (* XXX(dinosaure): assert (_ = true); *)
    let* () = write_lock_or_restart p need_to_restart in
    if !need_to_restart
    then ( let* () = delete addr' (_header_length + 4 + (Addr.length * 4)) in
           write_unlock addr )
    else
      let* () = clflush addr' _sizeof_n4 false true in
      let* () = update_child p kp (Addr.to_rdonly addr') in
      let* () = write_unlock p in
      let* () = write_unlock_and_obsolete addr in
      let* uid = atomic_get Addr.(addr + _header_owner) Value.leintnat in
      collect addr ~len:(_header_length + 4 + (Addr.length * 4)) ~uid

  let insert_compact_n16 (N16 addr as n16) p k kp value need_to_restart =
    let* prefix, prefix_count = get_prefix addr in
    let* level = get Addr.(addr + _header_depth) Value.leint31 in
    let* N16 addr' as n16' = alloc_n16 ~prefix ~prefix_count ~level in
    let* () = copy_n16_into_n16 n16 n16' in
    let* _  = add_child_n16 n16' k value false in
    (* XXX(dinosaure): assert (_ = true); *)
    let* () = write_lock_or_restart p need_to_restart in
    if !need_to_restart
    then ( let* () = delete addr' (_header_length + 16 + (Addr.length * 16)) in
           write_unlock addr )
    else
      let* () = clflush addr' _sizeof_n16 false true in
      let* () = update_child p kp (Addr.to_rdonly addr') in
      let* () = write_unlock p in
      let* () = write_unlock_and_obsolete addr in
      let* uid = atomic_get Addr.(addr + _header_owner) Value.leintnat in
      collect addr ~len:(_header_length + 16 + (Addr.length * 16)) ~uid

  let insert_compact_n48 (N48 addr as n48) p k kp value need_to_restart =
    let* prefix, prefix_count = get_prefix addr in
    let* level = get Addr.(addr + _header_depth) Value.leint31 in
    let* N48 addr' as n48' = alloc_n48 ~prefix ~prefix_count ~level in
    let* () = copy_n48_into_n48 n48 n48' in
    let* _  = add_child_n48 n48' k value false in
    (* XXX(dinosaure): assert (_ = true); *)
    let* () = write_lock_or_restart p need_to_restart in
    if !need_to_restart
    then ( let* () = delete addr' (_header_length + 256 + (Addr.length * 48)) in
           write_unlock addr )
    else
      let* () = clflush addr' _sizeof_n48 false true in
      let* () = update_child p kp (Addr.to_rdonly addr') in
      let* () = write_unlock p in
      let* () = write_unlock_and_obsolete addr in
      let* uid = atomic_get Addr.(addr + _header_owner) Value.leintnat in
      collect addr ~len:(_header_length + 256 + (Addr.length * 48)) ~uid

  let insert_and_unlock n p k kp value need_to_restart =
    let* ty = get_type n in
    match ty with
    | 0 ->
      let* compact_count =
        atomic_get Addr.(n + _header_compact_count) Value.leint16 in
      let* count = atomic_get Addr.(n + _header_count) Value.leint16 in
      Log.debug (fun m -> m "insert %02x into N4 (compact_count: %d, count: %d)"
                    k compact_count count) ;
      if compact_count = 4 && count <= 3
      then insert_compact_n4 (N4 n) p k kp value need_to_restart
      else _insert_grow_n4_n16 (N4 n) p k kp value need_to_restart
    | 1 ->
      let* compact_count =
        atomic_get Addr.(n + _header_compact_count) Value.leint16 in
      let* count = atomic_get Addr.(n + _header_count) Value.leint16 in
      if compact_count = 16 && count <= 14
      then insert_compact_n16 (N16 n) p k kp value need_to_restart
      else _insert_grow_n16_n48 (N16 n) p k kp value need_to_restart
    | 2 ->
      let* compact_count =
        atomic_get Addr.(n + _header_compact_count) Value.leint16 in
      let* count = atomic_get Addr.(n + _header_count) Value.leint16 in
      if compact_count = 48 && count <> 48
      then insert_compact_n48 (N48 n) p k kp value need_to_restart
      else _insert_grow_n48_n256 (N48 n) p k kp value need_to_restart
    | 3 ->
      (* TODO(dinosaure): [P-ART] uses [insertCompact<N256>] and [ROWEX]
         just do a simple [insert]. I'm not sure about this part! *)
      let* _  = add_child_n256 (N256 n) k value true in
      let* () = write_unlock n in
      return ()
    | _ -> assert false

  exception Duplicate

  let check_or_raise_duplicate ~level:off a b =
    Log.debug (fun m -> m "check duplicate ~level:%d %S %S" off a b) ;
    if String.length a = String.length b
    then ( let idx = ref (String.length a - 1) in
           while !idx >= off && a.[!idx] = b.[!idx] do decr idx done ;
           if !idx < off then raise Duplicate )

  let rec insert root key leaf =
    let rec restart () = insert root key leaf

    (* XXX(dinosaure): with [multicore] (eg. ['a t = 'a]), it should be posible
       to raise an exception [Restart] and simulate a [goto] as ROWEX explains.
       However, if we took the monadic-view of ['a t], [Restart] will leak.

       So we call [restart] and ensure that the call is tail-recursive (and can
       be optimised by OCaml). Then, we compile with [-unbox-closures] to avoid
       allocation on this area - but we need to introspect such optimisation. *)

    and _insert (node : [> `Rd | `Wr ] Addr.t) parent pk level =
      let need_to_restart = ref false in
      let* version = get_version node in
      let* res = check_prefix_pessimistic node ~key level in
      ( match res with
      | Skipped_level -> restart ()
      | No_match { non_matching_key; non_matching_prefix; level= level'; } ->
        Log.debug (fun m -> m "check_prefix_pessimistic %016x ~key:%S %d : \
                               No_match { non_matching_key: %c; \
                               non_matching_prefix: %S; level: %d }"
                      (node :> int) key level non_matching_key
                      non_matching_prefix level') ;
        if level' > String.length key then raise Duplicate ;
        (* XXX(dinosaure): such [if] is may be wrong... TODO! *)
        let* _version = lock_version_or_restart node version need_to_restart in
        if !need_to_restart then (restart[@tailcall]) () else
        let* prefix, _ = get_prefix node in
        Log.debug (fun m -> m "prefix of the current node %016x: %S" (node :> int) prefix) ;
        let* N4 addr as n4 =
          alloc_n4 ~prefix ~prefix_count:(level' - level) ~level:level' in
        let* _             = add_child_n4 n4 (Char.code key.![level']) leaf
            false in
        let* _             = add_child_n4 n4 (Char.code non_matching_key)
            (Addr.to_rdonly node) false in
        let* ()            = clflush addr _sizeof_n4 false true in
        let* ()            = write_lock_or_restart parent need_to_restart in
        if !need_to_restart
        then
          let* () = delete addr (_header_length + 4 + (Addr.length * 4)) in
          let* () = write_unlock node in
          (restart[@tailcall]) ()
        else
          (* XXX(dinosaure): red-zone, it's not possible to install a new node
             and truncate the prefix in a single operation. As a consequence, a
             reader may see the intermediate state. To solve this problem, we
             augment each node with a 'level' field, which stores the hieght of
             the node including the prefix and which never changes after a node
             has been created.

             With this additional information, the intermediate state is safe,
             because **the reader will detect that the prefix has to be
             skipped**. Similarly, it is also possible that a reader sees the
             final state without seen the new node. In that situation, the
             reader can detect the missing prefix using level field and
             retrieve the missing key from database.

             With P-ART, the C++ code tries to simulate the inconsistence with
             the macro CRASH_SPLIT. We did not integrate it here. *)
          let* () = update_child parent pk (Addr.to_rdonly addr) in
          let* () = write_unlock parent in
          let* _, prefix_count = get_prefix node in
          Log.debug (fun m -> m "set-prefix: level':%d" level') ;
          Log.debug (fun m -> m "set-prefix: level :%d" level ) ;
          Log.debug (fun m -> m "set-prefix: prefix-count:%d" prefix_count) ;
          Log.debug (fun m -> m "set-prefix %016x ~prefix:%S ~prefix_count:%d" (node :> int) non_matching_prefix (prefix_count - ((level' - level) + 1))) ;
          let* () = set_prefix node ~prefix:non_matching_prefix
              ~prefix_count:(prefix_count - ((level' - level) + 1)) true in
          let* () = write_unlock node in
          return ()
      | Match { level= level' } ->
        Log.debug (fun m -> m "check_prefix_pessimistic %016x ~key:%S %d : \
                               Match { %d }"
                              (node :> int) key level level') ;
        let level = level' in
        let* next = find_child node key.![level] in
        Log.debug (fun m -> m "child is null: %b." (Addr.is_null next)) ;
        if Addr.is_null next
        then
          let* _version =
            lock_version_or_restart node version need_to_restart in
          if !need_to_restart then (restart[@tailcall]) () else
            ( let* () = insert_and_unlock node parent (Char.code key.![level])
                  pk leaf need_to_restart in
              if !need_to_restart then (restart[@tailcall]) ()
              else return () )
        else if (next :> int) land 1 = 1
        then
          let () = Log.debug (fun m -> m "the child is a leaf.") in
          let* key' = get (Leaf.prj (Addr.unsafe_to_leaf next))
              Value.c_string in
          check_or_raise_duplicate ~level:(level + 1) key key' ;
          (* XXX(dinosaure): in the C impl., this check does **not** exists but:
             - create ()
             - insert "foo" 0
             - insert "foo" 1
             seems to work. So, the check try find the diff between [key] and
             [key'] from the end of these strings. The worst case is when
             [key = key'] of course but we should assume that the user does not
             want to insert several times the same key. *)
          let* _version =
            lock_version_or_restart node version need_to_restart in
          if !need_to_restart then (restart[@tailcall]) () else
          ( let prefix = Bytes.make _prefix '\000' in
            let prefix_count = ref 0 in
            let top = min (String.length key - (level + 1))
                (String.length key' - (level + 1)) in
            while !prefix_count < top
                  && key.[level + 1 + !prefix_count] =
                     key'.[level + 1 + !prefix_count]
            do if !prefix_count < 4
              then Bytes.set prefix !prefix_count
                  key.[level + 1 + !prefix_count] ;
              incr prefix_count done ;
            Log.debug (fun m -> m "prefix:%S (count: %d)"
                          (Bytes.unsafe_to_string prefix) !prefix_count) ;
            let* N4 addr as n4 =
              alloc_n4 ~prefix:(Bytes.unsafe_to_string prefix)
                ~prefix_count:!prefix_count
                ~level:(level + 1 + !prefix_count) in
            (* XXX(dinosaure): Imagine you add "foo" and "fo" into an empty tree
               (see [ctor]), we have: 1) a prefix "o" 2) an alteration between
               the end of "fo" and the last "o" of "foo". In that case, we must
               have an ~illegal~ access on these strings - fortunately, OCaml
               always pads a string with at least, one '\000'. So even if it
               seems an unsafe access, it is ~safe~ in this **specific**
               context. *)
            let* _   = add_child_n4 n4
                (Char.code key.![level + 1 + !prefix_count]) leaf false in
            let* _   = add_child_n4 n4
                (Char.code key'.![level + 1 + !prefix_count]) next false in
            let* () = clflush addr _sizeof_n4 false true in
            Log.debug (fun m -> m "Update key.[%d] = %c." level key.[level]) ;
            let* _   = update_child node
                (Char.code key.[level]) (Addr.to_rdonly addr) in
            let* _   = write_unlock node in
            return () )
        else
          let () = Log.debug (fun m -> m "the child is a node.") in
          _insert (Addr.of_int_rdwr (next :> int)) node
            (Char.code key.[level]) (succ level) ) in

    (* XXX(dinosaure): [ctor] creates a [N256] node on the [root]. So, the case
       to enlarge the current [root] node **can not** appears and the "parent"
       should not be set. In that case, it's ~safe~ to consider at the beginning
       [parent] as a [Addr.null] address.

       NOTE(dinosaure): this is the **only** case where we need to /cast/
       [Addr.null] to a [[ `Rd | `Wr ] Addr.t] value - to have a write access to
       [Addr.null]. According to the comment below, this write access should not
       be used to write something into [Addr.null] but we need to play the game
       of the type-system. *)

    _insert root Addr.(of_int_rdwr (null :> int)) 0 0

  let ctor () =
    let* N256 addr = alloc_n256 ~prefix:"" ~prefix_count:0 ~level:0 in
    return (addr)

  let insert root key value =
    Log.debug (fun m -> m "Insert %S." key) ;
    let len = (String.length key + size_of_word) / size_of_word in (* padding *)
    let len = len * size_of_word in
    let pad = String.make (len - String.length key) '\000' in
    let value = leintnat_to_string value in
    let* leaf = allocate ~kind:`Leaf [ key; pad; value ] in
    let* () = insert root key (Addr.unsafe_of_leaf (Leaf.inj leaf)) in
    Log.debug (fun m -> m "%S inserted." key) ; return ()

  [@@@warning "-32"]

  module Ringbuffer = struct
    let src = Logs.Src.create "ring"
    module Log = (val Logs.src_log src : Logs.LOG)

    type order = int

    let size_of_word = Sys.word_size / 8

    let cache_shift = 7
    let min_ptr = cache_shift - 3

    let empty = lnot 0
    let power_of_two ~order = 1 lsl order [@@inline]

    let map idx order n =
      (((idx land (n - 1)) asr (order + 1 - min_ptr))
       lor ((idx lsl min_ptr) land (n - 1)))
    [@@inline]

    let _tail = 0
    let _head = size_of_word
    let _threshold = _head + size_of_word
    let _array = _threshold + size_of_word

    let _pow2 order = 1 lsl order [@@inline]

    let rec _enqueue_loop ~order ~non_empty ring e_idx =
      let half = _pow2 order in
      let n = half * 2 in
      let* tail = fetch_add ~memory_order:Acq_rel Addr.(ring + _tail)
          Value.leintnat 1 in
      let t_cycle = (tail lsl 1) lor (2 * n - 1) in
      let* entry = atomic_get ~memory_order:Acquire
          Addr.(ring + _array + ((map tail order n) * size_of_word))
          Value.leintnat in

      (_enqueue_retry[@tailcall]) ~order ~non_empty
        ring tail t_cycle entry e_idx

    and _enqueue ~order ~non_empty ring tail t_cycle t_idx entry e_idx =
      let entry = ref entry in
      let* res = compare_exchange ~weak:true
          ~m0:Acq_rel ~m1:Acquire
          Addr.(ring + _array + (t_idx * size_of_word)) Value.leintnat
          entry (t_cycle lxor e_idx) in
      if not res
      then (_enqueue_retry[@tailcall]) ~order ~non_empty
          ring tail t_cycle !entry e_idx
      else
        ( Log.debug (fun m -> m "ring[%8x] <- %8x ^ %d = %8x"
                        t_idx t_cycle e_idx (t_cycle lxor e_idx)) ;
          let* threshold = atomic_get Addr.(ring + _threshold) Value.leintnat in
          let threshold' = (1 lsl order) + ((1 lsl order) * 2) - 1 in
          if not non_empty && threshold <> threshold'
          then atomic_set Addr.(ring + _threshold) Value.leintnat threshold'
          else return () )

    and _enqueue_retry ~order ~non_empty ring tail t_cycle entry e_idx =
      Log.debug (fun m -> m "enqueue retry") ;
      let half = _pow2 order in
      let n = half * 2 in
      let t_idx = map tail order n in
      let e_cycle = entry lor (2 * n - 1) in
      if e_cycle - t_cycle < 0 && entry = e_cycle
      then (_enqueue[@tailcall]) ~order ~non_empty
          ring tail t_cycle t_idx entry e_idx
      else
        let* head = atomic_get ~memory_order:Acquire Addr.(ring + _head)
            Value.leintnat in
        if e_cycle - t_cycle < 0 && entry = e_cycle lxor n && head - tail <= 0
        then (_enqueue[@tailcall]) ~order ~non_empty
            ring tail t_cycle t_idx entry e_idx
        else (_enqueue_loop[@tailcall]) ~order ~non_empty ring e_idx

    let enqueue ~order ~non_empty ring e_idx =
      Log.debug (fun m -> m "enqueue %d" e_idx) ;
      let half = _pow2 order in
      let n = half * 2 in
      _enqueue_loop ~order ~non_empty ring (e_idx lxor (n - 1))

    let rec _catchup ring tail head =
      Log.debug (fun m -> m "catchup") ;
      let tail = ref tail in
      let* res = compare_exchange
          ~weak:true Addr.(ring + _tail) Value.leintnat
          tail head
          ~m0:Acq_rel ~m1:Acquire in
      if not res
      then
        let* head = atomic_get Addr.(ring + _head) Value.leintnat in
        let* tail = atomic_get Addr.(ring + _tail) Value.leintnat in
        if tail - head >= 0 then return ()
        else (_catchup[@tailcall]) ring tail head
      else return ()

    let rec _dequeue_1 ~order ~non_empty ring head =
      if not non_empty
      then
        ( let* tail = atomic_get Addr.(ring + _tail) Value.leintnat
              ~memory_order:Acquire in
          if tail - (head + 1) <= 0
          then
            let* () = _catchup ring tail (head + 1) in
            let* _  = fetch_sub Addr.(ring + _threshold) Value.leintnat 1
                ~memory_order:Acq_rel in
            return (lnot 0)
          else
            let* res = fetch_sub Addr.(ring + _threshold) Value.leintnat 1
                ~memory_order:Acq_rel in
            if res <= 0 then return (lnot 0)
            else (_dequeue_0[@tailcall]) ~order ~non_empty ring )
      else (_dequeue_0[@tailcall]) ~order ~non_empty ring

    and _dequeue_while
        ~order ~non_empty ~attempt ring head h_cycle h_idx entry =
      let n = (1 lsl (order + 1)) in
      let e_cycle = entry lor (2 * n - 1) in
      Log.debug (fun m -> m "e-cycle: %16x" e_cycle) ;
      Log.debug (fun m -> m "h-cycle: %16x" h_cycle) ;
      if e_cycle = h_cycle
      then
        let* res = fetch_or
            Addr.(ring + _array + (h_idx * size_of_word)) Value.leintnat (n - 1)
            ~memory_order:Acq_rel in
        Log.debug (fun m -> m "ring[%8x] |= %8x <- %d (old: %8x)"
                      h_idx (n - 1) res entry) ;
        Log.debug (fun m -> m "return %d" (entry land (n - 1))) ;
        return (entry land (n - 1))
      else if entry lor n <> e_cycle
      then
        let entry_new = entry land (lnot n) in
        if entry = entry_new
        then (_dequeue_1[@tailcall]) ~order ~non_empty ring head
        else
          ( if e_cycle - h_cycle < 0
            then
              let entry = ref entry in
              let* res = compare_exchange ~weak:true
                  Addr.(ring + _array + (h_idx * size_of_word))
                  Value.leintnat entry entry_new
                  ~m0:Acq_rel ~m1:Acquire in
              if res then _dequeue_while ~order ~non_empty ~attempt
                  ring head h_cycle h_idx !entry
              else (_dequeue_1[@tailcall]) ~order ~non_empty ring head
            else (_dequeue_1[@tailcall]) ~order ~non_empty ring head )
      else
        ( let attempt = succ attempt in
          if attempt <= 10_000
          then
            (_dequeue_again[@tailcall]) ~order ~non_empty ~attempt
              ring head h_cycle h_idx
          else
            let entry_new = h_cycle lxor ((lnot entry) land n) in
            if e_cycle < h_cycle
            then
              let entry = ref entry in
              let* res = compare_exchange ~weak:true
                  Addr.(ring + _array + (h_idx * size_of_word))
                  Value.leintnat entry entry_new
                  ~m0:Acq_rel ~m1:Acquire in
              if res
              then (_dequeue_while[@tailcall]) ~order ~non_empty ~attempt
                  ring head h_cycle h_idx !entry
              else (_dequeue_1[@tailcall]) ~order ~non_empty ring head
            else (_dequeue_1[@tailcall]) ~order ~non_empty ring head )

    and _dequeue_again ~order ~non_empty ~attempt ring head h_cycle h_idx =
      let* entry = atomic_get
          Addr.(ring + _array + (h_idx * size_of_word))
          Value.leintnat ~memory_order:Acquire in
      Log.debug (fun m -> m "ring[%8x] = %8x" h_idx entry) ;
      (_dequeue_while[@tailcall]) ~order ~non_empty ~attempt
        ring head h_cycle h_idx entry

    and _dequeue_0 ~order ~non_empty ring =
      let n = (1 lsl (order + 1)) in
      let* head = fetch_add Addr.(ring + _head) Value.leintnat 1
          ~memory_order:Acq_rel in
      let h_cycle = (head lsl 1) lor (2 * n - 1) in
      let h_idx = map head order n in
      (_dequeue_again[@tailcall]) ~order ~non_empty ~attempt:0
        ring head h_cycle h_idx

    let dequeue ~order ~non_empty ring =
      Log.debug (fun m -> m "dequeue") ;
      let* threshold = atomic_get Addr.(ring + _threshold) Value.leintnat in
      if not non_empty && threshold < 0 then return (lnot 0)
      else (_dequeue_0[@tailcall]) ~order ~non_empty ring

    let peek ~order ~non_empty ring =
      Log.debug (fun m -> m "peek") ;
      let* threshold = atomic_get Addr.(ring + _threshold) Value.leintnat in
      if not non_empty && threshold < 0 then return (lnot 0)
      else
        let n = (1 lsl (order + 1)) in
        let* head = atomic_get Addr.(ring + _head) Value.leintnat
            ~memory_order:Acq_rel in
        let h_cycle = (head lsl 1) lor (2 * n - 1) in
        let h_idx = map head order n in
        let* entry = atomic_get
            Addr.(ring + _array + (h_idx * size_of_word))
            Value.leintnat ~memory_order:Acquire in
        Log.debug (fun m -> m "ring[%8x] = %8x" h_idx entry) ;
        let e_cycle = entry lor (2 * n - 1) in
        Log.debug (fun m -> m "e-cycle: %16x" e_cycle) ;
        Log.debug (fun m -> m "h-cycle: %16x" h_cycle) ;
        if e_cycle = h_cycle
        then return (entry land (n - 1))
        else return (lnot 0)

    let is_empty ring =
      let* threshold = atomic_get Addr.(ring + _threshold) Value.leintnat in
      return (not (threshold < 0))

    let order = 15
    let order_of_int x = x
    let size_of_order order =
      (size_of_word * 3) + (size_of_word lsl (order + 1))
  end
end
