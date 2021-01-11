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
type key = private string

val key : string -> key
external unsafe_key : string -> key = "%identity"

val make : unit -> 'a t

val insert : 'a t -> key -> 'a -> unit

val find : 'a t -> key -> 'a
(** [find x t] returns the current binding of [x] in [t], or raises [Not_found]
   if no such binding exists. *)

val find_opt : 'a t -> key -> 'a option

val pp : 'a Fmt.t -> 'a t Fmt.t

val minimum : 'a t -> key * 'a

val remove : 'a t -> key -> unit
