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

val pp_value : Format.formatter -> ('c, 'a) value -> unit
val pp_of_value : ('c, 'a) value -> Format.formatter -> 'a -> unit

type 'c memory_order =
  | Relaxed : [< `Rd | `Wr ] memory_order
  | Seq_cst : [< `Rd | `Wr ] memory_order
  | Release : [< `Wr ] memory_order
  | Acq_rel : [< `Rd | `Wr ] memory_order
  | Acquire : [< `Rd ] memory_order

val pp_memory_order : Format.formatter -> 'c memory_order -> unit

(*
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
  | Bind : 'a t * ('a -> 'b t) -> 'b t
  | Return : 'a -> 'a t
*)

type 'a fmt = Format.formatter -> 'a -> unit

(* val pp : 'a t fmt *)

module type S = sig
  type 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t

  val atomic_get : ?memory_order:[< `Rd ] memory_order -> [> `Rd ] Addr.t -> ([ `Atomic ], 'a) value -> 'a t
  val atomic_set : ?memory_order:[< `Wr ] memory_order -> [> `Wr ] Addr.t -> ([ `Atomic ], 'a) value -> 'a -> unit t
  val fetch_add  : ?memory_order:[< `Rd | `Wr ] memory_order -> [> `Rd | `Wr ] Addr.t -> ([ `Atomic ], int) value -> int -> int t
  val fetch_or   : ?memory_order:[< `Rd | `Wr ] memory_order -> [> `Rd | `Wr ] Addr.t -> ([ `Atomic ], int) value -> int -> int t
  val fetch_sub  : ?memory_order:[< `Rd | `Wr ] memory_order -> [> `Rd | `Wr ] Addr.t -> ([ `Atomic ], int) value -> int -> int t

  val compare_exchange :
    ?m0:[< `Rd | `Wr ] memory_order ->
    ?m1:[< `Rd | `Wr ] memory_order ->
    ?weak:bool ->
    [> `Rd | `Wr ] Addr.t ->
    ([ `Atomic ], 'a) value ->
    'a ref -> 'a -> bool t

  val pause_intrinsic : unit t

  val get : [> `Rd ] Addr.t -> ('c, 'a) value -> 'a t

  val allocate : kind:[ `Leaf | `Node ] -> ?len:int -> string list -> [ `Rd | `Wr ] Addr.t t
  val delete : _ Addr.t -> int -> unit t
  val collect : _ Addr.t -> len:int -> uid:int -> unit t
end

module Make (S : S) : sig
  open S

  type formatter

  val formatter : commit:(unit -> unit S.t) -> Format.formatter -> formatter
  val pp : formatter -> [> `Rd ] Addr.t -> unit t
  val find : [> `Rd ] Addr.t -> key -> int t
  val insert : [> `Rd | `Wr ] Addr.t -> key -> int -> unit t
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

  (** / *)

  type pessimistic =
    | Match of { level : int }
    | Skipped_level
    | No_match of { non_matching_key : char
                  ; non_matching_prefix : string
                  ; level : int }

  val check_prefix_pessimistic : [> `Rd ] Addr.t -> key:string -> int -> pessimistic t
  val check_prefix : [> `Rd ] Addr.t -> key:string -> key_len:int -> int -> int t
  val find_child : [> `Rd ] Addr.t -> char -> [ `Rd ] Addr.t t
end

(** / **)

val _header_owner : int
