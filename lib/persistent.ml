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

external rdtsc : unit -> int = "caml_rdtsc" [@@noalloc]

external clflush : int -> unit = "caml_clflush" [@@noalloc]

external sfence : unit -> unit = "caml_sfence" [@@noalloc]

external stream_int : memory -> int -> int -> unit = "caml_stream_int" [@@noalloc]

external atomic_compare_exchange_strong
  : memory -> int -> int ref -> int -> (_ memory_order * _ memory_order) -> bool
  = "caml_atomic_compare_exchange_strong_leuintnat" [@@noalloc]

external atomic_compare_exchange_weak
  : memory -> int -> int ref -> int -> (_ memory_order * _ memory_order) -> bool
  = "caml_atomic_compare_exchange_weak_leuintnat" [@@noalloc]

external get_c_string
  : memory -> int -> string
  = "caml_get_c_string"

external get_leint31
  : memory -> int -> int
  = "caml_get_leint31" [@@noalloc]

external get_leintnat
  : memory -> int -> int
  = "caml_get_leintnat" [@@noalloc]

external to_memory
  : (_, _, Bigarray.c_layout) Bigarray.Array1.t -> memory
  = "caml_to_memory" [@@noalloc]

[@@@warning "-30"]

type 'fd mmu =
  { mutable brk : int
  ; memory : memory
  ; ipc : Ipc.t
  ; write : 'fd -> string -> off:int -> len:int -> int -> unit
  ; free : (int, free_cell list) Hashtbl.t
  ; keep : (int, keep_cell list) Hashtbl.t
  ; readers : int Hashset.t }
and free_cell =
  { addr : int
  ; time : int }
and keep_cell =
  { addr : int
  ; len : int }
type 'fd write = 'fd -> string -> off:int -> len:int -> int -> unit

[@@@warning "+30"]

let append tbl k cell =
  try let vs = Hashtbl.find tbl k in Hashtbl.replace tbl k (cell :: vs)
  with Not_found -> Hashtbl.add tbl k [ cell ]

let append_keep_cell mmu ~time ~addr ~len =
  append mmu.keep time { addr; len; }

let append_free_cell mmu ~len ~addr ~time =
  append mmu.free len { addr; time; }

let mmu_of_memory ~write ipc memory =
  let brk = atomic_get_leuintnat memory 0 Seq_cst in
  { brk; memory; ipc; write
  ; free= Hashtbl.create 0x100
  ; keep= Hashtbl.create 0x100
  ; readers= Hashset.create 0x100 }

let memory_of_mmu { memory; _ } = memory

let size_of_word = Sys.word_size / 8

let root_of_mmu { memory; _ } =
  let addr = atomic_get_leuintnat memory size_of_word Seq_cst in
  Addr.of_int_rdwr addr

let ipc_of_mmu { ipc; _ } = ipc

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

let src = Logs.Src.create "persistent"
module Log = (val Logs.src_log src : Logs.LOG)

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
  | Rdtsc : int t
  | Clflush : [ `Wr ] Addr.t -> unit t
  | Sfence : unit t
  | Stream_int : [ `Wr ] Addr.t * int -> unit t
  | Bind : 'a t * ('a -> 'b t) -> 'b t
  | Return : 'a -> 'a t

let pf = Format.fprintf

let pp : type a. a t fmt = fun ppf v ->
  let open Rowex in match v with
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
  | Rdtsc -> pf ppf "rdtsc"
  | Clflush addr -> pf ppf "clflush %016x" (addr :> int)
  | Sfence -> pf ppf "sfence"
  | Stream_int (addr, v) -> pf ppf "movnt64 %016x %x" (addr :> int) v
  | Bind (Allocate (_, _, len), _) ->
     pf ppf "allocate %d byte(s) >>= fun _ ->" len
  | Bind _ -> pf ppf ">>="
  | Return _ -> pf ppf "return *"

let ( <.> ) f g = fun x -> f (g x)

module S = struct
  type nonrec 'a t = 'a t

  let bind x f = Bind (x, f)
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

  let rdtsc = Rdtsc

  let clflush addr = Clflush addr

  let sfence = Sfence

  let stream_int addr v = Stream_int (addr, v)

  let allocate ~kind ?len payloads =
    let len = match len with
      | Some len -> len
      | None -> List.fold_right (( + ) <.> String.length) payloads 0 in
    Allocate (kind, payloads, len)

  let delete addr len = Delete (addr, len)

  let collect addr ~len ~uid = Collect (addr, len, uid)

  let pp = pp
end

include Make(S)

let pp ppf addr = pp (formatter ~commit:(fun () -> S.return ()) ppf) addr

let free_cells mmu time =
  try
    let cells = Hashtbl.find mmu.keep time in
    List.iter (fun { addr; len; } -> append_free_cell mmu ~len ~addr ~time) cells
  with _ -> ()

(* TODO(dinosaure): replace it by a UNIX socket. DONE by [ipc/ipc.ml]. *)

(* XXX(dinosaure): [collect] must be protected by a global lock if we use
   multiple writers and one [ringbuffer]. However, to be able to have
   multiple readers and multiple writes, we associate one [ringbuffer]
   for each writer. *)
let collect ({ ipc; _ } as mmu) =
  Log.debug (fun m -> m "collect") ;
  let rec mark_and_sweep () =
    if Ipc.is_empty ipc
    then ()
    else
      let res = Int64.to_int (Ipc.dequeue ipc) in
      if Hashset.mem mmu.readers res
      then ( Hashset.remove mmu.readers res
           ; free_cells mmu res
           ; mark_and_sweep () )
      else ( Hashset.add mmu.readers res ; mark_and_sweep () ) in
  mark_and_sweep ()

let older_reader { ipc; _ } =
  Int64.to_int (Ipc.dequeue ipc)

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
  then ( let time = if Ipc.is_empty mmu.ipc then 0 else older_reader mmu in
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
  if Ipc.is_empty mmu.ipc
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
    | cmd -> Log.debug (fun m -> m "%a" S.pp cmd) in
  match cmd with
  | Atomic_get (memory_order, addr, Int8) ->
     atomic_get_uint8 memory (addr :> int) memory_order
  | Atomic_set (memory_order, addr, Int8, v) ->
     atomic_set_uint8 memory (addr :> int) memory_order v
  | Atomic_get (memory_order, addr, LEInt) ->
     atomic_get_leuintnat memory (addr :> int) memory_order
  | Atomic_set (memory_order, addr, LEInt, v) ->
     atomic_set_leuintnat memory (addr :> int) memory_order v
  | Atomic_get (memory_order, addr, LEInt16) ->
     atomic_get_leuint16 memory (addr :> int) memory_order
  | Atomic_set (memory_order, addr, LEInt16, v) ->
     atomic_set_leuint16 memory (addr :> int) memory_order v
  | Atomic_get (memory_order, addr, LEInt31) ->
     atomic_get_leuint31 memory (addr :> int) memory_order
  | Atomic_set (memory_order, addr, LEInt31, v) ->
     atomic_set_leuint31 memory (addr :> int) memory_order v
  | Atomic_get (memory_order, addr, LEInt64) ->
     atomic_get_leuint64 memory (addr :> int) memory_order
  | Atomic_set (memory_order, addr, LEInt64, v) ->
     atomic_set_leuint64 memory (addr :> int) memory_order v
  | Atomic_get (memory_order, addr, LEInt128) ->
    let res = Bytes.create 16 in
    atomic_get_leuint128 memory (addr :> int) memory_order res ;
    Bytes.unsafe_to_string res
  | Atomic_get (memory_order, addr, Addr_rd) ->
    Addr.of_int_rdonly (atomic_get_leuintnat memory (addr :> int) memory_order)
  | Atomic_set (memory_order, addr, Addr_rd, v) ->
    atomic_set_leuintnat memory (addr :> int) memory_order (v :> int)
  | Fetch_add (memory_order, addr, LEInt16, v) ->
    atomic_fetch_add_leuint16 memory (addr :> int) memory_order v
  | Fetch_add (memory_order, addr, LEInt, v) ->
    atomic_fetch_add_leuintnat memory (addr :> int) memory_order v
  | Fetch_sub (memory_order, addr, LEInt, v) ->
    atomic_fetch_sub_leuintnat memory (addr :> int) memory_order v
  | Fetch_or (memory_order, addr, LEInt, v) ->
    atomic_fetch_or_leuintnat memory (addr :> int) memory_order v
  | Pause_intrinsic -> pause_intrinsic ()
  | Compare_exchange (addr, LEInt, a, b, true, m0, m1) ->
    atomic_compare_exchange_weak memory (addr :> int) a b (m0, m1)
  | Compare_exchange (addr, LEInt, a, b, false, m0, m1) ->
    atomic_compare_exchange_strong memory (addr :> int) a b (m0, m1)
  | Get (addr, C_string) ->
    let res = get_c_string memory (addr :> int) in
    Log.debug (fun m -> m "Get %S." res) ; res
  | Get (addr, LEInt31) -> get_leint31 memory (addr :> int)
  | Get (addr, LEInt) -> get_leintnat memory (addr :> int)
  | Clflush addr -> clflush (addr :> int)
  | Stream_int (addr, v) -> stream_int memory (addr :> int) v
  | Sfence -> sfence ()
  | Rdtsc -> rdtsc ()
  | Return v -> v
  | Bind (Allocate (kind, payloads, len), f) ->
    let len' = List.fold_left (fun a x -> String.length x + a) 0 payloads in
    assert (len = len') ;
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
  | cmd -> invalid_arg "Invalid operation: %a" S.pp cmd
