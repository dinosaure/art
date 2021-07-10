open Rowex

type memory = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type 'a t

type 'fd mmu

type 'fd write = 'fd -> string -> off:int -> len:int -> int -> unit

val mmu_of_memory :
  write:'fd write ->
  Ipc.t -> memory -> 'fd mmu

val memory_of_mmu : 'fd mmu -> memory
val root_of_mmu : 'fd mmu -> [ `Rd | `Wr ] Addr.t
val ipc_of_mmu : 'fd mmu -> Ipc.t

val run : 'fd mmu -> 'a t -> 'a

val pp : Format.formatter -> [> `Rd ] Addr.t -> unit t
val find : [> `Rd ] Addr.t -> key -> int t
val insert : [> `Rd | `Wr ] Addr.t -> key -> int -> unit t
val ctor : unit -> [ `Rd | `Wr ] Addr.t t

(** / **)

external atomic_set_leuintnat
  : memory -> int -> _ memory_order -> int -> unit
  = "caml_atomic_set_leuintnat" [@@noalloc]

external atomic_get_leuintnat
  : memory -> int -> _ memory_order -> int
  = "caml_atomic_get_leuintnat" [@@noalloc]

external to_memory
  : (_, _, Bigarray.c_layout) Bigarray.Array1.t -> memory
  = "caml_to_memory" [@@noalloc]
