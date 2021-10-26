open Rowex

type 'c capabilities =
  | Reader : int64 -> ro capabilities
  | Writer : rdwr capabilities

let reader uid = Reader uid
let writer = Writer

type 'c opened = | constraint 'c = < .. >
type closed = |

type 'v state =
  | Opened : 'c Persistent.mmu * 'c capabilities -> 'c opened state
  | Closed : closed state

let closed = Closed

type ('p, 'q, 'a) t =
  | Return : 'a -> ('p, 'p, 'a) t
  | Bind : ('p, 'q, 'a) t * ('a -> ('q, 'r, 'b) t) -> ('p, 'r, 'b) t
  | Open : 'c capabilities * string -> (closed, 'c opened, unit) t
  | Create : string * int -> (closed, closed, (unit, [> `Msg of string ]) result) t
  | Close : ('c opened, closed, unit) t
  | Find : key -> ('c rd opened, 'c rd opened, int) t
  | Insert : key * int -> (rdwr opened, rdwr opened, unit) t

let return x = Return x
let open_index c ~path = Open (c, path)
let find key = Find key
let insert key value = Insert (key, value)
let close = Close
let bind x f = Bind (x, f)
let create ?(len= 2_097_152) path = Create (path, len)
let ( let* ) = bind

let page_size = 4096 (* TODO(dinosaure): replace it by a call. *)

let rec run
  : type a p q. p state -> (p, q, a) t -> q state * a
  = fun s m -> match m, s with
  | Return x, _ -> s, x
  | Bind (m, f), _ -> let s, x = run s m in run s (f x)
  | Find key, Opened (mmu, capabilities) ->
    Opened (mmu, capabilities), Persistent.(run mmu (Persistent.find mmu key))
  | Insert (key, value), Opened (mmu, capabilities) ->
    Opened (mmu, capabilities), Persistent.(run mmu (insert mmu key value))
  | Open (Reader uid, path), Closed ->
    let fd = Unix.openfile path Unix.[ O_RDWR ] 0o644 in
    let len = ((Unix.fstat fd).st_size + page_size) / page_size in (* XXX(dinosaure): padding. *)
    let len = len * page_size in
    let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.char Bigarray.c_layout true [| len |] in
    let memory = Bigarray.array1_of_genarray memory in
    let ipc = Ipc.connect (Fmt.str "%s.socket" path) in
    let mmu = Persistent.ro ipc memory in
    Unix.close fd ; Ipc.enqueue ipc uid ; Opened (mmu, Reader uid), ()
  | Close, Opened (mmu, Reader uid) ->
    let ipc = Persistent.ipc mmu in
    Ipc.enqueue ipc uid ; Closed, ()
  | Open (Writer, path), Closed ->
    let fd = Unix.openfile path Unix.[ O_RDWR ] 0o644 in
    let len = ((Unix.fstat fd).st_size + page_size) / page_size in (* XXX(dinosaure): padding. *)
    let len = len * page_size in
    let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.char Bigarray.c_layout true [| len |] in
    let memory = Bigarray.array1_of_genarray memory in
    let ipc = Ipc.connect (Fmt.str "%s.socket" path) in
    let mmu = Persistent.rdwr ipc memory in
    Unix.close fd ; Opened (mmu, Writer), ()
  | Create (path, len), Closed ->
    let fd = Unix.openfile path Unix.[ O_CREAT; O_RDWR ] 0o644 in
    let _  = Unix.lseek fd len Unix.SEEK_SET in
    let len = (len + page_size) / page_size in
    let len = len * page_size in
    let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.char Bigarray.c_layout true [| len |] in
    let memory = Bigarray.array1_of_genarray memory in
    ( match Ipc.create (Fmt.str "%s.socket" path) with
    | Ok () ->
      let ipc = Ipc.connect (Fmt.str "%s.socket" path) in
      let _mmu = Persistent.rdwr ipc memory in
      let _mmu = Persistent.run _mmu (Persistent.make ipc memory) in
      Ipc.close ipc ; Unix.close fd ; Closed, Ok ()
    | Error err -> Closed, Error err )
  | Close, Opened (_mmu, Writer) -> Closed, ()   

(*
let create ?(len= 1_048_576) filename =
  let fd = Unix.openfile filename Unix.[ O_CREAT; O_RDWR ] 0o644 in
  let _  = Unix.lseek fd len Unix.SEEK_SET in
  let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.int Bigarray.c_layout true [| len |] in
  let memory = Bigarray.array1_of_genarray memory in
  let memory = to_memory memory in
  let brk    = size_of_word * 2 in
  atomic_set_leuintnat memory 0 brk ;
  Rresult.R.failwith_error_msg  ;
  let ipc    = Ipc.connect (Fmt.str "%s.socket" filename) in
  let mmu    = mmu_of_memory ipc memory in
  let root   = run mmu (Persistent.ctor ()) in
  atomic_set_leuintnat memory size_of_word (root :> int) ;
  Ipc.close ipc ; Unix.close fd


let wr_mmu_of_file filename =
 let fd = Unix.openfile filename Unix.[ O_RDWR ] 0o644 in
 let len = ((Unix.fstat fd).st_size + page_size) / page_size in (* XXX(dinosaure): padding. *)
 let len = len * page_size in
 let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.char Bigarray.c_layout true [| len |] in
 let memory = Bigarray.array1_of_genarray memory in
 let ipc = Ipc.connect (Fmt.str "%s.socket" filename) in
 let mmu = mmu_of_memory ipc memory in
 let root = atomic_get_leuintnat (memory_of_mmu mmu) size_of_word in
 Unix.close fd ; MMU mmu, Addr.of_int_to_rdwr root

let delete_reader ipc = append_reader ipc

let rd_mmu_of_file filename =
  let fd = Unix.openfile filename Unix.[ O_RDWR ] 0o644 in
  let len = ((Unix.fstat fd).st_size + page_size) / page_size in (* XXX(dinosaure): padding. *)
  let len = len * page_size in
  let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.char Bigarray.c_layout true [| len |] in
  let memory = Bigarray.array1_of_genarray memory in
  let ipc = Ipc.connect (Fmt.str "%s.socket" filename) in
  let mmu = mmu_of_memory ipc memory in
  let root = atomic_get_leuintnat (memory_of_mmu mmu) size_of_word in
  Unix.close fd ; append_reader ipc ; MMU mmu, Addr.of_int_to_rdonly root

let insert (MMU mmu, root) key v = run mmu (Persistent.insert root (Rowex.unsafe_key key) v)
let lookup (MMU mmu, root) key = run mmu (Persistent.find root (Rowex.unsafe_key key))
let pp ppf (MMU mmu, root) = run mmu (Persistent.pp ppf root)
let ipc (MMU mmu, _root) = Persistent.ipc_of_mmu mmu

let unsafe_mmu_of_file filename =
  let fd = Unix.openfile filename Unix.[ O_RDWR ] 0o644 in
  let len = ((Unix.fstat fd).st_size + page_size) / page_size in (* XXX(dinosaure): padding. *)
  let len = len * page_size in
  let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.char Bigarray.c_layout true [| len |] in
  let memory = Bigarray.array1_of_genarray memory in
  let ipc = Ipc.connect (Fmt.str "%s.socket" filename) in
  let mmu = mmu_of_memory ipc memory in
  let root = atomic_get_leuintnat (memory_of_mmu mmu) size_of_word in
  Unix.close fd ; MMU mmu, Addr.of_int_to_rdwr root
*)
