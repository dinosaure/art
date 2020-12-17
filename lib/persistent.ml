let () = Printexc.record_backtrace true

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
  : memory -> int -> _ memory_order -> int -> int
  = "caml_atomic_fetch_add_leuint16" [@@noalloc]

external atomic_fetch_add_leuintnat
  : memory -> int -> _ memory_order -> int -> int
  = "caml_atomic_fetch_add_leuintnat" [@@noalloc]

external atomic_fetch_sub_leuintnat
  : memory -> int -> _ memory_order -> int -> int
  = "caml_atomic_fetch_sub_leuintnat" [@@noalloc]

external atomic_fetch_or_leuintnat
  : memory -> int -> _ memory_order -> int -> int
  = "caml_atomic_fetch_or_leuintnat" [@@noalloc]

external pause_intrinsic : unit -> unit = "caml_pause_intrinsic" [@@noalloc]

external atomic_compare_exchange_strong
  : memory -> int -> int ref -> int -> (_ memory_order * _ memory_order) -> bool
  = "caml_atomic_compare_exchange_strong_leuintnat" [@@noalloc]

external atomic_compare_exchange_weak
  : memory -> int -> int ref -> int -> (_ memory_order * _ memory_order) -> bool
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

external to_memory
  : (_, _, Bigarray.c_layout) Bigarray.Array1.t -> memory
  = "caml_to_memory" [@@noalloc]

[@@@warning "-30"]

type 'fd mmu =
  { mutable brk : int
  ; memory : memory
  ; ringbuffer : 'fd * memory
  ; sync : 'fd -> unit
  ; free : (int, free_cell list) Hashtbl.t
  ; keep : (int, keep_cell list) Hashtbl.t
  ; readers : int Hashset.t }
and free_cell =
  { addr : int
  ; time : int }
and keep_cell =
  { addr : int
  ; len : int }

[@@@warning "+30"]

let append tbl k cell =
  try let vs = Hashtbl.find tbl k in Hashtbl.replace tbl k (cell :: vs)
  with Not_found -> Hashtbl.add tbl k [ cell ]

let append_keep_cell mmu ~time ~addr ~len =
  append mmu.keep time { addr; len; }

let append_free_cell mmu ~len ~addr ~time =
  append mmu.free len { addr; time; }

let mmu_of_memory ~sync fd ~ring memory =
  let brk = atomic_get_leuintnat memory 0 Seq_cst in
  { brk; memory; ringbuffer= fd, ring; sync
  ; free= Hashtbl.create 0x100
  ; keep= Hashtbl.create 0x100
  ; readers= Hashset.create 0x100 }

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

type ring = memory

let rec rrun : type a. ring -> a t -> a = fun memory cmd ->
  let () = match cmd with
    | Bind _ | Return _ -> ()
    | cmd -> Log.debug (fun m -> m "%a" pp cmd) in
  match cmd with
  | Atomic_get (memory_order, addr, BEInt) ->
     atomic_get_leuintnat memory (addr :> int) memory_order
  | Atomic_set (memory_order, addr, BEInt, v) ->
     atomic_set_leuintnat memory (addr :> int) memory_order v
  | Fetch_add (memory_order, addr, BEInt, v) ->
    atomic_fetch_add_leuintnat memory (addr :> int) memory_order v
  | Fetch_sub (memory_order, addr, BEInt, v) ->
    atomic_fetch_sub_leuintnat memory (addr :> int) memory_order v
  | Fetch_or (memory_order, addr, BEInt, v) ->
    atomic_fetch_or_leuintnat memory (addr :> int) memory_order v
  | Compare_exchange (addr, BEInt, a, b, true, m0, m1) ->
    atomic_compare_exchange_weak memory (addr :> int) a b (m0, m1)
  | Compare_exchange (addr, BEInt, a, b, false, m0, m1) ->
    atomic_compare_exchange_strong memory (addr :> int) a b (m0, m1)
  | Pause_intrinsic -> pause_intrinsic ()
  | Return v -> v
  | Bind (v, f) -> let v = rrun memory v in rrun memory (f v)
  | cmd -> invalid_arg "Invalid operation: %a" pp cmd

let free_cells mmu time =
  try
    let cells = Hashtbl.find mmu.keep time in
    List.iter (fun { addr; len; } -> append_free_cell mmu ~len ~addr ~time) cells
  with _ -> ()

(* XXX(dinosaure): [collect] must be protected by a global lock if we use
   multiple writers and one [ringbuffer]. However, to be able to have
   multiple readers and multiple writes, we associate one [ringbuffer]
   for each writer. *)
let collect ({ ringbuffer= fd, memory; _ } as mmu) =
  Log.debug (fun m -> m "collect") ;
  let zero = Addr.of_int_rdwr 0 in
  let rec mark_and_sweep fd memory =
    mmu.sync fd ;
    let res = rrun memory Ringbuffer.(dequeue ~order:order ~non_empty:false zero) in
    if res = lnot 0 then ()
    else if Hashset.mem mmu.readers res
    then ( Hashset.remove mmu.readers res
         ; free_cells mmu res
         ; mark_and_sweep fd memory )
    else ( Hashset.add mmu.readers res ; mark_and_sweep fd memory ) in
  mark_and_sweep fd memory

let older_reader ({ ringbuffer= fd, memory; _ } as mmu) =
  let zero = Addr.of_int_rdwr 0 in
  mmu.sync fd ;
  let res = rrun memory Ringbuffer.(dequeue ~order:order ~non_empty:false zero) in
  if res = lnot 0 then 0 else res

let _header_owner = Rowex._header_owner

(* TODO(dinosaure): a special case exists when we don't have
   a ringbuffer ([Bigarray.Array1.dim (snd mmu.ringbuffer)].
   We should delete such case and use properly a ringbuffer
   even if we have one writer/one reader. *)

let ralloc mmu ~kind len payloads =
  let brk = 1 + ((mmu.brk - 1) / 8) in
  let brk = brk * 8 in (* XXX(dinosaure): align memory. *)
  Logs.debug (fun m -> m "brk:%016x, allocate %d byte(s)" brk len) ;
  if brk + len <= Bigarray.Array1.dim mmu.memory
  then ( let time = if Bigarray.Array1.dim (snd mmu.ringbuffer) = 0 then 0 else older_reader mmu in
         blitv payloads mmu.memory brk
       ; atomic_set_leuintnat mmu.memory 0 Seq_cst (brk + len)
       ; if kind = `Node then atomic_set_leuintnat mmu.memory (brk + _header_owner) Seq_cst time
       ; Logs.debug (fun m -> m "brk:%016x" (brk + len))
       ; mmu.brk <- brk + len
       ; Addr.of_int_rdwr brk )
  else failwith "Out of memory: %d + %d <= %d" brk len
    (Bigarray.Array1.dim mmu.memory)

(* XXX(dinosaure): second chance or allocate *)
let alloc mmu ~kind len payloads =
  Log.debug (fun m -> m "alloc[1]") ;
  try match Hashtbl.find mmu.free len with
    | [] -> raise Not_found
    | cell :: tl ->
      let time = older_reader mmu in
      Hashtbl.replace mmu.free len tl ;
      blitv payloads mmu.memory cell.addr ;
      if kind = `Node then atomic_set_leuintnat mmu.memory (cell.addr + _header_owner) Seq_cst time ;
      Addr.of_int_rdwr cell.addr
  with Not_found -> ralloc mmu ~kind len payloads

(* XXX(dinosaure): first chance or collect *)
let alloc mmu ~kind len payloads =
  Log.debug (fun m -> m "alloc[0]") ;
  let _, ring = mmu.ringbuffer in
  if Bigarray.Array1.dim ring = 0
  then ralloc mmu ~kind len payloads
  else
    try match Hashtbl.find mmu.free len with
      | [] -> raise Not_found
      | cell :: tl ->
        let time = older_reader mmu in
        Hashtbl.replace mmu.free len tl ;
        blitv payloads mmu.memory cell.addr ;
        if kind = `Node then atomic_set_leuintnat mmu.memory (cell.addr + _header_owner) Seq_cst time ;
        Addr.of_int_rdwr cell.addr
    with Not_found -> collect mmu ; alloc mmu ~kind len payloads

let rec run : type fd a. fd mmu -> a t -> a = fun ({ memory; _ } as mmu) cmd ->
  let () = match cmd with
    | Bind _ | Return _ -> ()
    | cmd -> Log.debug (fun m -> m "%a" pp cmd) in
  match cmd with
  | Atomic_get (memory_order, addr, Int8) ->
     atomic_get_uint8 memory (addr :> int) memory_order
  | Atomic_set (memory_order, addr, Int8, v) ->
     atomic_set_uint8 memory (addr :> int) memory_order v
  | Atomic_get (memory_order, addr, BEInt) ->
     atomic_get_leuintnat memory (addr :> int) memory_order
  | Atomic_set (memory_order, addr, BEInt, v) ->
     atomic_set_leuintnat memory (addr :> int) memory_order v
  | Atomic_get (memory_order, addr, BEInt16) ->
     atomic_get_leuint16 memory (addr :> int) memory_order
  | Atomic_set (memory_order, addr, BEInt16, v) ->
     atomic_set_leuint16 memory (addr :> int) memory_order v
  | Atomic_get (memory_order, addr, BEInt31) ->
     atomic_get_leuint31 memory (addr :> int) memory_order
  | Atomic_set (memory_order, addr, BEInt31, v) ->
     atomic_set_leuint31 memory (addr :> int) memory_order v
  | Atomic_get (memory_order, addr, BEInt64) ->
     atomic_get_leuint64 memory (addr :> int) memory_order
  | Atomic_set (memory_order, addr, BEInt64, v) ->
     atomic_set_leuint64 memory (addr :> int) memory_order v
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
  | Fetch_sub (memory_order, addr, BEInt, v) ->
    atomic_fetch_sub_leuintnat memory (addr :> int) memory_order v
  | Fetch_or (memory_order, addr, BEInt, v) ->
    atomic_fetch_or_leuintnat memory (addr :> int) memory_order v
  | Pause_intrinsic -> pause_intrinsic ()
  | Compare_exchange (addr, BEInt, a, b, true, m0, m1) ->
    atomic_compare_exchange_weak memory (addr :> int) a b (m0, m1)
  | Compare_exchange (addr, BEInt, a, b, false, m0, m1) ->
    atomic_compare_exchange_strong memory (addr :> int) a b (m0, m1)
  | Get (addr, C_string) ->
    let res = get_c_string memory (addr :> int) in
    Log.debug (fun m -> m "Get %S.\n" res) ; res
  | Get (addr, BEInt31) -> get_beint31 memory (addr :> int)
  | Get (addr, BEInt) -> get_beintnat memory (addr :> int)
  | Return v -> v
  | Bind (Allocate (kind, payloads, len), f) ->
    let addr = alloc mmu ~kind len payloads in
    run mmu (f addr)
  | Collect _ -> ()
  | Bind (Delete (addr, len), f) ->
    append_free_cell mmu ~len ~addr:(addr :> int) ~time:0 ;
    run mmu (f ())
  | Bind (Collect (addr, len, uid), f) ->
    append_keep_cell mmu ~time:uid ~addr:(addr :> int) ~len ;
    run mmu (f ())
  | Bind (v, f) -> let v = run mmu v in run mmu (f v)
  | cmd -> invalid_arg "Invalid operation: %a" pp cmd

