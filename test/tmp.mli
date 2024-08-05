val default_dir : unit -> Fpath.t
val set_default_dir : Fpath.t -> unit

type pattern = (string -> string, Format.formatter, unit, string) format4

val tmp :
  ?mode:int -> ?dir:Fpath.t -> pattern -> (Fpath.t, [> `Msg of string ]) result
