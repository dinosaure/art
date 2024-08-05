external is_cpu_clflush_present : unit -> bool = "is_cpu_clflush_present"
external is_cpu_clflushopt_present : unit -> bool = "is_cpu_clflushopt_present"
external is_cpu_clwb_present : unit -> bool = "is_cpu_clwb_present"

open Configurator.V1.C_define.Value

let __aarch64__ = "__aarch64__"

let _ =
  let c = Configurator.V1.create "sse" in
  let defines =
    Configurator.V1.C_define.import c ~includes:[] [ (__aarch64__, Switch) ]
  in
  match List.assoc_opt __aarch64__ defines with
  | Some (Switch true) ->
      Format.printf "dc cvac:    true\n%!";
      let flags = [ "-DART_DC_CVAC" ] in
      Configurator.V1.Flags.write_sexp "flush.sexp" flags
  | _ ->
      let clflush = is_cpu_clflush_present () in
      let clflushopt = is_cpu_clflushopt_present () in
      let clwb = is_cpu_clwb_present () in

      Format.printf "clflush:    %b\n%!" clflush;
      Format.printf "clflushopt: %b\n%!" clflushopt;
      Format.printf "clwb:       %b\n%!" clwb;

      let flags = [] in
      let flags = if clflush then "-DART_CLFLUSH" :: flags else flags in
      let flags = if clflushopt then "-DART_CLFLUSHOPT" :: flags else flags in
      let flags = if clwb then "-DART_CLWB" :: flags else flags in
      Configurator.V1.Flags.write_sexp "flush.sexp" flags
