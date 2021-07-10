open Rowex
open Persistent

let size_of_word = Sys.word_size / 8

let ignore_pwrite _ _ ~off:_ ~len:_ _ = ()

external pwrite
  : Unix.file_descr -> string -> off:int -> len:int -> int -> unit
  = "caml_pwrite"
(* XXX(dinosaure): [uerror] allocates. *)

type memory_manager_unit = MMU : 'cap mmu -> memory_manager_unit
type 'a t = memory_manager_unit * 'a Addr.t constraint 'a = [< `Rd | `Wr ]

let create ?(len= 1_048_576) filename =
  let fd = Unix.openfile filename Unix.[ O_CREAT; O_RDWR ] 0o644 in
  let _  = Unix.lseek fd len Unix.SEEK_SET in
  let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.int Bigarray.c_layout true [| len |] in
  let memory = Bigarray.array1_of_genarray memory in
  let memory = to_memory memory in
  let brk    = size_of_word * 2 in
  atomic_set_leuintnat memory 0 Seq_cst brk ;
  Rresult.R.failwith_error_msg (Ipc.create (Fmt.strf "%s.socket" filename)) ;
  let ipc    = Ipc.connect (Fmt.strf "%s.socket" filename) in
  let mmu    = mmu_of_memory ~write:ignore_pwrite ipc memory in
  let root   = run mmu (Persistent.ctor ()) in
  atomic_set_leuintnat memory size_of_word Seq_cst (root :> int) ;
  Ipc.close ipc ; Unix.close fd

let page_size = 4096

let wr_mmu_of_file filename =
 let fd = Unix.openfile filename Unix.[ O_RDWR ] 0o644 in
 let len = ((Unix.fstat fd).st_size + page_size) / page_size in (* XXX(dinosaure): padding. *)
 let len = len * page_size in
 let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.char Bigarray.c_layout true [| len |] in
 let memory = Bigarray.array1_of_genarray memory in
 let ipc = Ipc.connect (Fmt.strf "%s.socket" filename) in
 let mmu = mmu_of_memory ~write:pwrite ipc memory in
 let root = atomic_get_leuintnat (memory_of_mmu mmu) size_of_word Seq_cst in
 Unix.close fd ; MMU mmu, Addr.of_int_rdwr root

let append_reader ipc =
  Ipc.enqueue ipc (Int64.of_int (Unix.getpid ()))
;;

let delete_reader ipc = append_reader ipc

let rd_mmu_of_file filename =
  let fd = Unix.openfile filename Unix.[ O_RDWR ] 0o644 in
  let len = ((Unix.fstat fd).st_size + page_size) / page_size in (* XXX(dinosaure): padding. *)
  let len = len * page_size in
  let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.char Bigarray.c_layout true [| len |] in
  let memory = Bigarray.array1_of_genarray memory in
  let ipc = Ipc.connect (Fmt.strf "%s.socket" filename) in
  let mmu = mmu_of_memory ~write:pwrite ipc memory in
  let root = atomic_get_leuintnat (memory_of_mmu mmu) size_of_word Seq_cst in
  Unix.close fd ; append_reader ipc ; MMU mmu, Addr.of_int_rdonly root

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
  let ipc = Ipc.connect (Fmt.strf "%s.socket" filename) in
  let mmu = mmu_of_memory ~write:ignore_pwrite ipc memory in
  let root = atomic_get_leuintnat (memory_of_mmu mmu) size_of_word Seq_cst in
  Unix.close fd ; MMU mmu, Addr.of_int_rdwr root
