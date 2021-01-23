open Rowex

type memory = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type ring = memory

type 'a t

val rrun : ring -> 'a t -> 'a

type 'fd mmu

val mmu_of_memory : sync:('fd -> unit) -> 'fd -> ring:ring -> memory -> 'fd mmu
val memory_of_mmu : 'fd mmu -> memory
val root_of_mmu : 'fd mmu -> [ `Rd | `Wr ] Addr.t
val run : 'fd mmu -> 'a t -> 'a

val find : [ `Rd ] Addr.t -> key -> int t
val insert : [ `Rd | `Wr ] Addr.t -> key -> int -> unit t
val ctor : unit -> [ `Rd | `Wr ] Addr.t t

module Ringbuffer : sig
  type order = private int

  val enqueue : order:order -> non_empty:bool -> [ `Rd | `Wr ] Addr.t -> int -> unit t
  val dequeue : order:order -> non_empty:bool -> [ `Rd | `Wr ] Addr.t -> int t
  val peek : order:order -> non_empty:bool -> [ `Rd | `Wr ] Addr.t -> int t
  val is_empty : [ `Rd | `Wr ] Addr.t -> bool t

  val order : order
  val order_of_int : int -> order
  val size_of_order : order -> int
end

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
