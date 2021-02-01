module RB : sig
  type t

  val create : string -> unit
  val load : string -> t
end

type 'a t constraint 'a = [< `Rd | `Wr ]

val create : ?len:int -> string -> unit

val pp : [> `Rd ] t Fmt.t

val append_reader : RB.t -> unit
val delete_reader : RB.t -> unit

val wr_mmu_of_file : ring:RB.t -> string -> [ `Rd | `Wr ] t
val rd_mmu_of_file : ring:RB.t -> string -> [ `Rd ] t

val insert : [> `Wr | `Rd ] t -> string -> int -> unit
val lookup : [> `Rd ] t -> string -> int

(** / *)

val unsafe_mmu_of_file : string -> [ `Wr | `Rd ] t
