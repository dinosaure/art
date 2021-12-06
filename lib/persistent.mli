open Rowex

type memory = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type truncate = readers:int Hashset.t -> memory -> len:int64 -> memory
(** The type of functions to {i truncate}.

    [Persistent] implements its own allocator and the {!type:mmu} can request a
    larger file {i via} [truncate]. Be aware that, at this stage, we don't have
    any synchronization between the writer and readers.

    An usual call to [Unix.truncate] can lead a [SIGSEGV] to other processes.
    The user must implement a synchronization mechanism where readers must
    stop what they do, let the writer [truncate] the file and ask readers to
    {i remap} the file.

    {b Note}: {!val:ro} does not need an implementation of [truncate]. Only
    {!val:rdwr} is able (via {!val:insert}) to call [truncate]. *)

type 'a t

val pp : Format.formatter -> 'c rd Addr.t -> unit t

type 'c mmu

val ro : truncate:truncate -> Ipc.t -> memory -> ro mmu
val rdwr : truncate:truncate -> Ipc.t -> memory -> rdwr mmu

val find : 'c rd mmu -> key -> int t
val insert : rdwr mmu -> key -> int -> unit t
val make : truncate:truncate -> Ipc.t -> memory -> rdwr mmu t

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

val unsafe_set_memory : 'c mmu -> memory -> unit

module Hashset = Hashset
