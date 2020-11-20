open Rowex

type memory = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external atomic_get_uint8
  : memory -> int -> _ memory_order -> int
  = "caml_atomic_get_uint8" [@@noalloc]

external atomic_set_uint8
  : memory -> int -> _ memory_order -> int -> unit
  = "caml_atomic_set_uint8" [@@noalloc]

external atomic_get_leuintnat
  : memory -> int -> _ memory_order -> int
  = "caml_atomic_get_leuintnat" [@@noalloc]

external atomic_set_leuintnat
  : memory -> int -> _ memory_order -> int -> unit
  = "caml_atomic_set_leuintnat" [@@noalloc]

external atomic_get_leuint16
  : memory -> int -> _ memory_order -> int
  = "caml_atomic_get_leuint16" [@@noalloc]

external atomic_set_leuint16
  : memory -> int -> _ memory_order -> int -> unit
  = "caml_atomic_set_leuint16" [@@noalloc]

external atomic_get_leuint31
  : memory -> int -> _ memory_order -> int
  = "caml_atomic_get_leuint31" [@@noalloc]

external atomic_set_leuint31
  : memory -> int -> _ memory_order -> int -> unit
  = "caml_atomic_set_leuint31" [@@noalloc]    

external atomic_get_leuint64
  : memory -> int -> _ memory_order -> (int64[@unboxed])
  = "bytecode_compilation_not_supported"
    "caml_atomic_get_leuint64" [@@noalloc]

external atomic_set_leuint64
  : memory -> int -> _ memory_order -> (int64[@unboxed]) -> unit
  = "bytecode_compilation_not_supported"
    "caml_atomic_set_leuint64" [@@noalloc]

external atomic_get_leuint128
  : memory -> int -> _ memory_order -> bytes -> unit
  = "caml_atomic_get_leuint128" [@@noalloc]

external atomic_fetch_add_leuint16
  : memory -> int -> _ memory_order -> int -> unit
  = "caml_atomic_fetch_add_leuint16" [@@noalloc]

external atomic_fetch_add_leuintnat
  : memory -> int -> _ memory_order -> int -> unit
  = "caml_atomic_fetch_add_leuintnat" [@@noalloc]

external pause_intrinsic : unit -> unit = "caml_pause_intrinsic" [@@noalloc]

external atomic_compare_exchange_strong
  : memory -> int -> int -> int -> _ memory_order -> bool
  = "caml_atomic_compare_exchange_strong_leuintnat" [@@noalloc]

external atomic_compare_exchange_weak
  : memory -> int -> int -> int -> _ memory_order -> bool
  = "caml_atomic_compare_exchange_weak_leuintnat" [@@noalloc]

external get_c_string
  : memory -> int -> string
  = "caml_get_c_string"

external get_beint31
  : memory -> int -> int
  = "caml_get_beint31" [@@noalloc]

external get_beintnat
  : memory -> int -> int
  = "caml_get_beintnat" [@@noalloc]

type mmu =
  { mutable brk : int
  ; memory : memory
  ; free : (int * int) list }

let mmu_of_memory ?(free= []) memory =
  let brk = atomic_get_leuintnat memory 0 Seq_cst in
  { brk; memory; free; }

let memory_of_mmu { memory; _ } = memory

let size_of_word = Sys.word_size / 8

let root_of_mmu { memory; _ } =
  let addr = atomic_get_leuintnat memory size_of_word Seq_cst in
  Addr.of_int_rdwr addr

external bigarray_unsafe_set_uint8  : memory -> int -> int -> unit = "%caml_ba_set_1"
external bigarray_unsafe_set_uint32 : memory -> int -> int32 -> unit = "%caml_bigstring_set32"

external string_unsafe_get_uint32 : string -> int -> int32 = "%caml_string_get32"

let rec blitv payloads memory dst_off = match payloads with
  | hd :: tl ->
    let len = String.length hd in
    let len0 = len land 3 in
    let len1 = len asr 2 in
    for i = 0 to len1 - 1 do
      let i = i * 4 in
      let v = string_unsafe_get_uint32 hd i in
      bigarray_unsafe_set_uint32 memory (dst_off + i) v
    done ;
    for i = 0 to len0 - 1 do
      let i = len1 * 4 + i in
      bigarray_unsafe_set_uint8 memory (dst_off + i) (Char.code hd.[i])
    done ;
    blitv tl memory (dst_off + len)
  | [] -> ()

let failwith fmt = Format.kasprintf failwith fmt
let invalid_arg fmt = Format.kasprintf invalid_arg fmt

let src = Logs.Src.create "atomic"
module Log = (val Logs.src_log src : Logs.LOG)

let rec run : type a. mmu -> a t -> a = fun ({ brk; memory; free; } as mmu) cmd ->
  Log.debug (fun m -> m "%a" pp cmd) ;
  match cmd with
  | Atomic_get (memory_order, addr, Int8) -> atomic_get_uint8 memory (addr :> int) memory_order
  | Atomic_set (memory_order, addr, Int8, v) -> atomic_set_uint8 memory (addr :> int) memory_order v
  | Atomic_get (memory_order, addr, BEInt) -> atomic_get_leuintnat memory (addr :> int) memory_order
  | Atomic_set (memory_order, addr, BEInt, v) -> atomic_set_leuintnat memory (addr :> int) memory_order v
  | Atomic_get (memory_order, addr, BEInt16) -> atomic_get_leuint16 memory (addr :> int) memory_order
  | Atomic_set (memory_order, addr, BEInt16, v) -> atomic_set_leuint16 memory (addr :> int) memory_order v
  | Atomic_get (memory_order, addr, BEInt31) -> atomic_get_leuint31 memory (addr :> int) memory_order
  | Atomic_set (memory_order, addr, BEInt31, v) -> atomic_set_leuint31 memory (addr :> int) memory_order v
  | Atomic_get (memory_order, addr, BEInt64) -> atomic_get_leuint64 memory (addr :> int) memory_order
  | Atomic_set (memory_order, addr, BEInt64, v) -> atomic_set_leuint64 memory (addr :> int) memory_order v
  | Atomic_get (memory_order, addr, BEInt128) ->
    let res = Bytes.create 16 in
    atomic_get_leuint128 memory (addr :> int) memory_order res ;
    Bytes.unsafe_to_string res
  | Atomic_get (memory_order, addr, Addr_rd) ->
    Addr.of_int_rdonly (atomic_get_leuintnat memory (addr :> int) memory_order)
  | Atomic_set (memory_order, addr, Addr_rd, v) ->
    atomic_set_leuintnat memory (addr :> int) memory_order (v :> int)
  | Fetch_add (memory_order, addr, BEInt16, v) ->
    atomic_fetch_add_leuint16 memory (addr :> int) memory_order v
  | Fetch_add (memory_order, addr, BEInt, v) ->
    atomic_fetch_add_leuintnat memory (addr :> int) memory_order v
  | Pause_intrinsic -> pause_intrinsic ()
  | Compare_exchange (addr, BEInt, a, b, true, memory_order) ->
    atomic_compare_exchange_weak memory (addr :> int) a b memory_order
  | Compare_exchange (addr, BEInt, a, b, false, memory_order) ->
    atomic_compare_exchange_strong memory (addr :> int) a b memory_order
  | Get (addr, C_string) -> get_c_string memory (addr :> int)
  | Get (addr, BEInt31) -> get_beint31 memory (addr :> int)
  | Get (addr, BEInt) -> get_beintnat memory (addr :> int)
  | Return v -> v
  | Bind (Allocate (payloads, len), f) ->
    let brk = 1 + ((brk - 1) / 8) in
    let brk = brk * 8 in (* XXX(dinosaure): align memory. *)
    Logs.debug (fun m -> m "brk:%016x, allocate %d byte(s)" brk len) ;
    if brk + len <= Bigarray.Array1.dim memory
    then ( blitv payloads memory brk
         ; atomic_set_leuintnat memory 0 Seq_cst (brk + len)
         ; Logs.debug (fun m -> m "brk:%016x" (brk + len))
         ; mmu.brk <- brk + len
         ; run mmu (f (Addr.of_int_rdwr brk)) )
    else failwith "Out of memory: %d + %d <= %d" brk len (Bigarray.Array1.dim memory)
  | Collect _ -> ()
  | Bind (Delete (addr, len), f) ->
    run { brk; memory; free= (((addr :> int), len) :: free) } (f ())
  | Bind (Collect (addr, len), f) ->
    run { brk; memory; free= (((addr :> int), len) :: free) } (f ())
  | Bind (v, f) -> let v = run mmu v in run mmu (f v)
  | cmd -> invalid_arg "Invalid operation: %a" pp cmd
