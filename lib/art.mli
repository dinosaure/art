(** Adaptive Radix Tree in OCaml.

    Implementation of Adaptive Radix Tree (trie), for efficient
    indexing in memory. Its {i lookup} performance surpasses highly tuned,
    read-only search trees, while supporting very efficient insertions and
    deletions as well. ART is very space efficient and solves the problem
    of excessive worst-case space consumption, which plagues most radix tree,
    by adaptively choosing compact and efficient data structures for internal
    nodes. Even though, ART's performance is comparable to hash tables, it
    maintains the data in sorted order, which enables additional operations
    like range scan and prefix {i lookup}.
*)

type 'a t
(** The type of trees from type {!type:key} to type ['a]. *)

type key = private string
(** The type of the tree keys. A {i null-terminated} [string]. *)

val key : string -> key

external unsafe_key : string -> key = "%identity"

val make : unit -> 'a t
(** [make ()] creates a new, empty tree. *)

val insert : 'a t -> key -> 'a -> unit
(** [insert t k v] replaces the current binding of [k]
   in [t] by a binding of [k] to [v]. If [k] is unbound in [t],
   a binding of [k] to [v] is added to [t]. *)

val find : 'a t -> key -> 'a
(** [find x t] returns the current binding of [x] in [t], or raises [Not_found]
   if no such binding exists. *)

val find_opt : 'a t -> key -> 'a option
(** [find_opt x t] returns [Some v] if the current value of [x] in
   [t] is [v], or [None] if no binding for [x] exists. *)

val pp : 'a Fmt.t -> 'a t Fmt.t

val is_empty : 'a t -> bool
(** [is_empty tree] returns [true] if [tree] is empty. Otherwise, it returns
   [false]. *)

val minimum : 'a t -> key * 'a
(** Return the binding with the smallest {!type:key} in a given tree
   or raise [Invalid_argument] if the tree is empty. *)

val maximum : 'a t -> key * 'a
(** Same as {!minimum}, but returns the binding with the
   largest {!type:key} in the given tree. *)

val remove : 'a t -> key -> unit
(** [remove t k] removes the current binding of [k] in [t].
   It raises [Not_found] if [k] is not bound in [t]. *)

val iter : f:(key -> 'a -> 'acc -> 'acc) -> 'acc -> 'a t -> 'acc
(** [iter ~f a t] computes [(f kN dN ... (f k1 d1 a) ...)], where
   [k1 ... kN] are the keys of all bindings in [t] (in increasing order),
   and [d1 ... dN] are the associated data. *)

val of_seq : (key * 'a) Seq.t -> 'a t
(** Build a tree from the given bindings. *)

val to_seq : 'a t -> (key * 'a) Seq.t
(** Iterate on the whole map, in increasing order of keys. *)

val prefix_iter : prefix:key -> f:(key -> 'a -> 'acc -> 'acc) -> 'acc -> 'a t -> 'acc
