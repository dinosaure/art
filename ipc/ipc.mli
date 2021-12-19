(** {1:Inter-process communication via named pipe.}

    This module implement a really simple mechanism where multiple processes
    can communicate together. The idea is simple. We create an Unix named pipe
    which can be filled by some programs and some others (mostly a unique one)
    can consume the pipe.

    The data exchanged between processus is an [int64]. The process can produce
    /{!val:enqueue} an [int64] or it can consume/{!val:dequeue} an [int64].
    Unix gives to us the opportunity to check if the [ipc] is empty or not -
    and it is able to get this information without a consumption.

    Finally, the user can force a certain scheduling of processes via
    {!val:with_lock} where the process will try to acquire the lock on the
    [ipc], do something on it (or not), and release the lock for other
    processes. *)

type t

val is_empty : t -> bool
val dequeue : t -> int64
val enqueue : t -> int64 -> unit
val connect : string -> t
val close : t -> unit

val create : string -> (unit, [> `Msg of string ]) result

val with_lock : f:(t -> 'a) -> t -> 'a
