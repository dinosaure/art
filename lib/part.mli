(** {1: The UNIX implementation of [rowex].}

    [Part] is the UNIX implementation of [rowex] which explicitely requires
    [unix.cmxa] as a dependency as well as few {i syscalls} such as [msync(2)]
    and [getpagesize(2)].

    It provides a DSL which allows the user to mainly {!val:insert} and
    {!val:find} into an index file represented by {!val:state}. The DSL is a
    {i monad} {!val:t} which expects {i pre} and {i post} condition. Then, the
    user is able to {i prove} that the resource (the index) is correctly
    closed.

    We consider 2 views of the index:
    - the reader
    - the writer

    The user is able to create one writer and multiple readers. Then, he can
    describe {i via} the monad what he wants to do such as:
    {[
      let insert_from_input ic =
        let open Part in
        match Rowex.key (input_line ic) with
        | key ->
          let* () = insert key 666 in
          return (Ok ())
        | exception _ -> return (Error (`Msg "no input"))
     ]}

     The example above can be executed with {!val:run} only with an {i opened}
     {!type:state}. You can get an {i opened} {!type:state} with
     {!val:open_index} from a {i closed} one ({!val:closed}).

     By this way, we ensure that the user does not misuse resources.

     {2:Readers.}

     The user is able to launch multiple readers but something is needed to
     identify them. A {!val:reader} requires an [int64] value which must be
     unique (an other reader should {b not} have the same value). In that
     context, we advise the user to associate a reader has its PID with
     [Unix.getpid ()]. Of course, this context is perfect when you have
     multiple heavy processses - the user must find something else about
     light thread so.

     Of course, and this is the goal of [rowex], multiple readers can
     co-exist on the same index - and they can {!val:find} a value 
     in parallel from others. 

     {2:Writer.}

     Only one writer is allowed. The writer is mainly used to {!val:insert} but
     it can {!val:find} a value too. *)

open Rowex

type 'c capabilities

val reader : int64 -> ro capabilities
val writer : rdwr capabilities

type 'a opened constraint 'a = < .. >
type closed

type ('p, 'q, 'a) t
type 'v state

val closed : closed state

val return : 'a -> ('p, 'p, 'a) t
val open_index : 'c capabilities -> path:string -> (closed, 'c opened, unit) t
val find : key -> ('c rd opened, 'c rd opened, int) t
val insert : key -> int -> (rdwr opened, rdwr opened, unit) t
val close : ('c opened, closed, unit) t

val create : ?len:int -> string
  -> (closed, closed, (unit, [> `Msg of string ]) result) t

val bind : ('p, 'q, 'a) t -> ('a -> ('q, 'r, 'b) t) -> ('p, 'r, 'b) t

val run : 'p state -> ('p, 'q, 'a) t -> 'q state * 'a
val is_closed : 'p state -> bool

val ( let* ) : ('p, 'q, 'a) t -> ('a -> ('q, 'r, 'b) t) -> ('p, 'r, 'b) t
