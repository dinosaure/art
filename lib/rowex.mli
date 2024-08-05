exception Duplicate

(** Persistent implementation of Adaptive Radix Tree.

    This module implements the core of ROWEX/P-ART from the given
    way to atomically load and store values. This implementation wants to
    ensure 2 things:
    - [insert] and [lookup] can be executed in {b true} parallelism
    - persistence is ensured by required {i syscalls}
*)

type key = private string

val key : string -> key
external unsafe_key : string -> key = "%identity"

type 'a rd = < rd : unit ; .. > as 'a
type 'a wr = < wr : unit ; .. > as 'a
type ro = < rd : unit >
type wo = < wr : unit >
type rdwr = < rd : unit ; wr : unit >

module Addr : sig
  type 'a t = private int

  val null : rdwr t
  val is_null : 'a t -> bool
  external of_int_to_rdonly : int -> ro t = "%identity"
  external of_int_to_wronly : int -> wo t = "%identity"
  external of_int_to_rdwr : int -> rdwr t = "%identity"
  external to_wronly : 'a wr t -> wo t = "%identity"
  external to_rdonly : 'a rd t -> ro t = "%identity"
  external unsafe_to_int : _ t -> int = "%identity"
  val ( + ) : 'a t -> int -> 'a t
end

type ('c, 'a) value =
  | Int8 : (atomic, int) value
  | LEInt : (atomic, int) value
  | LEInt16 : (atomic, int) value
  | LEInt31 : (atomic, int) value
  | LEInt64 : (atomic, int64) value
  | LEInt128 : (atomic, string) value
  | Addr_rd : (atomic, ro Addr.t) value
  | Addr_rdwr : (atomic, rdwr Addr.t) value
  | C_string : (non_atomic, string) value

and atomic = Atomic
and non_atomic = Non_atomic

val pp_value : Format.formatter -> ('c, 'a) value -> unit
val pp_of_value : ('c, 'a) value -> Format.formatter -> 'a -> unit

type 'a fmt = Format.formatter -> 'a -> unit

module type S = sig
  type 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val atomic_get : 'a rd Addr.t -> (atomic, 'v) value -> 'v t
  val atomic_set : 'a wr Addr.t -> (atomic, 'v) value -> 'v -> unit t

  val persist : 'a wr Addr.t -> len:int -> unit t
  (** [persist addr ~len] forces the data to get written out to memory.
      Even if cache can be used to load some values, [persist] ensures that
      the value is really stored {b persistently} into the given destination
      such as we guarantee data validity despite power failures.

      More concretely, it should be (for [len <= word_size]):
      {[
        sfence
        clwb addr
        sfence
      ]}

      {b NOTE}: the first [sfence] is not systematically needed depending on
      what was done before (and if it's revelant for the current computation
      regardless the status of the cache) - such disposition is hard to track
      and we prefer to assume a correct write order than a micro-optimization.
    *)

  val fetch_add : rdwr Addr.t -> (atomic, int) value -> int -> int t
  val fetch_or : rdwr Addr.t -> (atomic, int) value -> int -> int t
  val fetch_sub : rdwr Addr.t -> (atomic, int) value -> int -> int t

  val compare_exchange :
    ?weak:bool ->
    rdwr Addr.t ->
    (atomic, 'a) value ->
    'a Atomic.t ->
    'a ->
    bool t

  val pause_intrinsic : unit t
  (** [pause_intrinsic] provides a hint to the processor that the code
      sequence is a spin-wait loop. *)

  val get : 'a rd Addr.t -> ('t, 'v) value -> 'v t

  (** This implementation needs:
      - an allocator [allocate]
      - where given addresses by it can be free-ed ([delete])
      - and let the allocator to {i stamp} the requested object by an [uid]
        which should help the allocator to recognize the owner of it

      [collect] means that the process which asks to collect the given
      address does not need anymore this memory block from {b its}
      point-of-view. So, from {b its} point-of-view, this block can be re-used
      for an other purpose. However, into a parallel context with multiple
      processes, this block can be in-used by someone else. In that case,
      [collect] defers the [delet]ion of it until nobodies need it.

      So, [uid] helps the allocator to ensure {i consistency} between multiple
      processes. *)

  val allocate :
    kind:[ `Leaf | `Node ] -> ?len:int -> string list -> rdwr Addr.t t

  val delete : _ Addr.t -> int -> unit t
  val collect : _ Addr.t -> len:int -> uid:int -> unit t
end

module Make (S : S) : sig
  open S

  type formatter

  val formatter : commit:(unit -> unit S.t) -> Format.formatter -> formatter
  val pp : formatter -> 'a rd Addr.t -> unit t
  val find : 'a rd Addr.t -> key -> int t
  val insert : rdwr Addr.t -> key -> int -> unit t
  val exists : 'a rd Addr.t -> key -> bool t
  val make : unit -> rdwr Addr.t t
  val remove : rdwr Addr.t -> key -> unit t

  (** / *)

  type pessimistic =
    | Match of { level : int }
    | Skipped_level
    | No_match of {
        non_matching_key : char;
        non_matching_prefix : string;
        level : int;
      }

  val check_prefix_pessimistic :
    'a rd Addr.t -> key:string -> int -> pessimistic t

  val check_prefix : 'a rd Addr.t -> key:string -> key_len:int -> int -> int t
  val find_child : 'a rd Addr.t -> char -> ro Addr.t t
end

(** / *)

val _header_owner : int
