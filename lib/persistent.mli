open Rowex

type mmu
type memory = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val mmu_of_memory : ?free:(int * int) list -> memory -> mmu
val memory_of_mmu : mmu -> memory
val root_of_mmu : mmu -> [ `Rd | `Wr ] Addr.t
val run : mmu -> 'a t -> 'a

type ring = memory

val rrun : ring -> 'a t -> 'a

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
