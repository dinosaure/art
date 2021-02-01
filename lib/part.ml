open Rowex
open Persistent

let empty = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0
let size_of_word = Sys.word_size / 8

let ignore_msync _ _ _ _ = ()
let ignore_pwrite _ _ ~off:_ ~len:_ _ = ()

external pwrite
  : Unix.file_descr -> string -> off:int -> len:int -> int -> unit
  = "caml_pwrite"
(* XXX(dinosaure): [uerror] allocates. *)

external msync
  : memory -> int -> int -> msync -> unit
  = "caml_msync"

module RB = struct
  type t = Unix.file_descr * memory

  let order = Ringbuffer.order

  let create filename =
    let len = Ringbuffer.size_of_order order in
    let fd  = Unix.openfile filename Unix.[ O_CREAT; O_RDWR ] 0o644 in
    let _   = Unix.lseek fd len Unix.SEEK_SET in
    let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.int Bigarray.c_layout true [| len |] in
    let memory = Bigarray.array1_of_genarray memory in
    let memory = to_memory memory in
    atomic_set_leuintnat memory (size_of_word * 0) Seq_cst 0 ;
    atomic_set_leuintnat memory (size_of_word * 1) Seq_cst 0 ;
    atomic_set_leuintnat memory (size_of_word * 2) Seq_cst (-1) ;
    for i = 0 to (1 lsl ((order :> int) + 1)) - 1 do
      atomic_set_leuintnat memory (size_of_word * (3 + i)) Seq_cst (-1)
    done ;
    Unix.close fd

  let load filename =
    let len = Ringbuffer.size_of_order order in
    let fd  = Unix.openfile filename Unix.[ O_CREAT; O_RDWR ] 0o644 in
    let _   = Unix.lseek fd len Unix.SEEK_SET in
    let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.int Bigarray.c_layout true [| len |] in
    let memory = Bigarray.array1_of_genarray memory in
    let memory = to_memory memory in
    fd, memory
end

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
  let mmu    = mmu_of_memory ~msync:ignore_msync ~write:ignore_pwrite () ~ring:empty memory in
  let root   = run mmu (Persistent.ctor ()) in
  atomic_set_leuintnat memory size_of_word Seq_cst (root :> int) ;
  Unix.close fd

let page_size = 4096

let wr_mmu_of_file ~ring:(fd_ring, memory_ring) filename =
 let fd = Unix.openfile filename Unix.[ O_RDWR ] 0o644 in
 let len = ((Unix.fstat fd).st_size + page_size) / page_size in (* XXX(dinosaure): padding. *)
 let len = len * page_size in
 let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.char Bigarray.c_layout true [| len |] in
 let memory = Bigarray.array1_of_genarray memory in
 let mmu = mmu_of_memory ~msync:msync ~write:pwrite fd_ring ~ring:memory_ring memory in
 let root = atomic_get_leuintnat (memory_of_mmu mmu) size_of_word Seq_cst in
 Unix.close fd ; MMU mmu, Addr.of_int_rdwr root

let append_reader (_fd_ring, ring) =
  let zero = Addr.of_int_rdwr 0 in
  msync ring 0 (Ringbuffer.size_of_order RB.order) SYNC ;
  rrun ring Ringbuffer.(enqueue ~order ~non_empty:false zero (Unix.getpid ())) ;
  msync ring 0 (Ringbuffer.size_of_order RB.order) SYNC ;
;;

let delete_reader (fd_ring, ring) = append_reader (fd_ring, ring)

let rd_mmu_of_file ~ring:(fd_ring, memory_ring) filename =
  let fd = Unix.openfile filename Unix.[ O_RDWR ] 0o644 in
  let len = ((Unix.fstat fd).st_size + page_size) / page_size in (* XXX(dinosaure): padding. *)
  let len = len * page_size in
  let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.char Bigarray.c_layout true [| len |] in
  let memory = Bigarray.array1_of_genarray memory in
  let mmu = mmu_of_memory ~msync:ignore_msync ~write:pwrite fd_ring ~ring:empty memory in
  let root = atomic_get_leuintnat (memory_of_mmu mmu) size_of_word Seq_cst in
  Unix.close fd ; append_reader (fd_ring, memory_ring) ; MMU mmu, Addr.of_int_rdonly root

let insert (MMU mmu, root) key v = run mmu (Persistent.insert root (Rowex.unsafe_key key) v)
let lookup (MMU mmu, root) key = run mmu (Persistent.find root (Rowex.unsafe_key key))
let pp ppf (MMU mmu, root) = run mmu (Persistent.pp ppf root)

let unsafe_mmu_of_file filename =
  let fd = Unix.openfile filename Unix.[ O_RDWR ] 0o644 in
  let len = ((Unix.fstat fd).st_size + page_size) / page_size in (* XXX(dinosaure): padding. *)
  let len = len * page_size in
  let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.char Bigarray.c_layout true [| len |] in
  let memory = Bigarray.array1_of_genarray memory in
  let mmu = mmu_of_memory ~msync:ignore_msync ~write:ignore_pwrite () ~ring:empty memory in
  let root = atomic_get_leuintnat (memory_of_mmu mmu) size_of_word Seq_cst in
  Unix.close fd ; MMU mmu, Addr.of_int_rdwr root
