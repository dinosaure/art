type 'a t

type 'a ivar

val return : 'a -> 'a t

val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

val ( >>> ) : unit t -> unit t -> unit t

val both : 'a t -> 'b t -> ('a * 'b) t

val fork : (unit -> 'a t) -> 'a ivar t

val fork_and_join : (unit -> 'a t) -> (unit -> 'b t) -> ('a * 'b) t

val fork_and_join_unit : (unit -> unit t) -> (unit -> unit t) -> unit t

val parallel_map : 'a list -> f:('a -> 'b t) -> 'b list t

val parallel_iter : 'a list -> f:('a -> unit t) -> unit t

val run_process : ?file:string -> (unit -> 'a) -> ('a, int) result t

val run : 'a t -> 'a

val set_concurrency : int -> unit

val get_concurrency : unit -> int
