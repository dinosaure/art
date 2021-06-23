type 'a t constraint 'a = [< `Rd | `Wr ]

val create : ?len:int -> string -> unit

val pp : [> `Rd ] t Fmt.t

val append_reader : Ipc.t -> unit
val delete_reader : Ipc.t -> unit

val wr_mmu_of_file : Ipc.t -> string -> [ `Rd | `Wr ] t
val rd_mmu_of_file : Ipc.t -> string -> [ `Rd ] t

val insert : [> `Wr | `Rd ] t -> string -> int -> unit
val lookup : [> `Rd ] t -> string -> int

(** / *)

val unsafe_mmu_of_file : Ipc.t -> string -> [ `Wr | `Rd ] t
