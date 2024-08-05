module Make (S : sig
  val memory : bytes
end) : Rowex.S with type 'a t = 'a
