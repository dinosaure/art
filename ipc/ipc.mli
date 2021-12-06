type t

val is_empty : t -> bool
val dequeue : t -> int64
val enqueue : t -> int64 -> unit
val connect : string -> t
val close : t -> unit

val create : string -> (unit, [> `Msg of string ]) result

val with_lock : f:(t -> 'a) -> t -> 'a
