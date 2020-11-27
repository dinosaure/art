(** Persistent implementation of Adaptive Radix Tree. *)

type key = private string

val key : string -> key
external unsafe_key : string -> key = "%identity"

module Addr : sig
  type -'a t = private int constraint 'a = [< `Rd | `Wr ] [@@immediate]

  val is_null : 'a t -> bool
  val null : [ `Rd ] t
  val length : int

  external of_int_rdonly : int -> [ `Rd ] t = "%identity"
  external of_int_wronly : int -> [ `Wr ] t = "%identity"
  external of_int_rdwr : int -> [ `Rd | `Wr ] t = "%identity"

  external to_wronly : [> `Wr ] t -> [ `Wr ] t = "%identity"
  external to_rdonly : [> `Rd ] t -> [ `Rd ] t = "%identity"

  val ( + ) : 'a t -> int -> 'a t
end

type ('c, 'a) value =
  | Int8     : ([ `Atomic ], int) value
  | BEInt    : ([ `Atomic ], int) value
  | BEInt16  : ([ `Atomic ], int) value
  | BEInt31  : ([ `Atomic ], int) value
  | BEInt64  : ([ `Atomic ], int64) value
  | BEInt128 : ([ `Atomic ], string) value
  | Addr_rd  : ([ `Atomic ], [ `Rd ] Addr.t) value
  | C_string : ([ `Non_atomic ], string) value

type 'c memory_order =
  | Relaxed : [< `Rd | `Wr ] memory_order
  | Seq_cst : [< `Rd | `Wr ] memory_order
  | Release : [< `Wr ] memory_order

type 'a t =
  | Atomic_get : [< `Rd ] memory_order * [> `Rd ] Addr.t * ([ `Atomic ], 'a) value -> 'a t
  | Atomic_set : [< `Wr ] memory_order * [> `Wr ] Addr.t * ([ `Atomic ], 'a) value * 'a -> unit t
  | Fetch_add  : [< `Rd | `Wr ] memory_order * [> `Rd | `Wr ] Addr.t * ([ `Atomic ], int) value * int -> unit t
  | Pause_intrinsic : unit t
  | Compare_exchange : [> `Rd | `Wr ] Addr.t *
                       ([ `Atomic ], 'a) value * 'a * 'a * bool * [< `Rd | `Wr ] memory_order -> bool t
  | Get : [> `Rd ] Addr.t * ('c, 'a) value -> 'a t
  | Allocate : string list * int -> [ `Rd | `Wr ] Addr.t t
  | Delete : _ Addr.t * int -> unit t
  | Collect : _ Addr.t * int -> unit t
  | Bind : 'a t * ('a -> 'b t) -> 'b t
  | Return : 'a -> 'a t

type 'a fmt = Format.formatter -> 'a -> unit

val pp : 'a t fmt

val find : [ `Rd ] Addr.t -> key -> int t
val insert : [ `Rd | `Wr ] Addr.t -> key -> int -> unit t
val ctor : unit -> [ `Rd | `Wr ] Addr.t t
