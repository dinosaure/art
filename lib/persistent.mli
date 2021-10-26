open Rowex

type memory = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type 'a t

val pp : Format.formatter -> 'c rd Addr.t -> unit t

type 'c mmu

val ro : Ipc.t -> memory -> ro mmu
val rdwr : Ipc.t -> memory -> rdwr mmu

val find : 'c rd mmu -> key -> int t
val insert : rdwr mmu -> key -> int -> unit t
val make : Ipc.t -> memory -> rdwr mmu t

val run : 'c mmu -> 'a t -> 'a
val ipc : 'c mmu -> Ipc.t

(** / **)

external atomic_set_leuintnat
  : memory -> int -> int -> unit
  = "caml_atomic_set_leuintnat" [@@noalloc]

external atomic_get_leuintnat
  : memory -> int -> int
  = "caml_atomic_get_leuintnat" [@@noalloc]

external to_memory
  : (_, _, Bigarray.c_layout) Bigarray.Array1.t -> memory
  = "caml_to_memory" [@@noalloc]
