type 'a t
type key = private string

val key : string -> key

val make : unit -> 'a t
val insert : 'a t -> key -> 'a -> unit
val find : 'a t -> key -> 'a
val find_opt : 'a t -> key -> 'a option
val pp : 'a Fmt.t -> 'a t Fmt.t
