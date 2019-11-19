type 'a t

val make : unit -> 'a t
val insert : 'a t -> string -> 'a -> unit
val find : 'a t -> string -> 'a
val find_opt : 'a t -> string -> 'a option
val pp : 'a Fmt.t -> 'a t Fmt.t
