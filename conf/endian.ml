external is_big_endian : unit -> bool = "caml_is_big_endian" [@@noalloc]

let _ =
  let flags =
    match is_big_endian () with true -> [ "-DART_BIG_ENDIAN" ] | false -> []
  in
  (* XXX(dinosaure): we assume, by default, a little endian
   * architecture. It seems a bit difficult to really check
   * if we are on a little-endian architecture but more easily
   * to check if we use a big-endian one. *)
  Configurator.V1.Flags.write_sexp "endian.sexp" flags
