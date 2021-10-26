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
val create : ?len:int -> string -> (closed, closed, (unit, [> `Msg of string ]) result) t
val bind : ('p, 'q, 'a) t -> ('a -> ('q, 'r, 'b) t) -> ('p, 'r, 'b) t

val run : 'p state -> ('p, 'q, 'a) t -> 'q state * 'a

val ( let* ) : ('p, 'q, 'a) t -> ('a -> ('q, 'r, 'b) t) -> ('p, 'r, 'b) t

(*
val create : ?len:int -> string -> unit

val pp : 'c rd t Fmt.t

val append_reader : Ipc.t -> unit
val delete_reader : Ipc.t -> unit

val wr_mmu_of_file : string -> rdwr t
val rd_mmu_of_file : string -> ro t

val insert : rdwr t -> string -> int -> unit
val lookup : 'c rd t -> string -> int
val ipc : _ t -> Ipc.t

(** / *)

val unsafe_mmu_of_file : string -> rdwr t
*)
