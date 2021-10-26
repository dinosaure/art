external is_cpu_clflush_present
  : unit -> bool
  = "is_cpu_clflush_present"

external is_cpu_clflushopt_present
  : unit -> bool
  = "is_cpu_clflushopt_present"

external is_cpu_clwb_present
  : unit -> bool
  = "is_cpu_clwb_present"

let _ =
  let clflush = is_cpu_clflush_present () in
  let clflushopt = is_cpu_clflushopt_present () in
  let clwb = is_cpu_clwb_present () in

  Format.printf "clflush:    %b\n%!" clflush ;
  Format.printf "clflushopt: %b\n%!" clflushopt ;
  Format.printf "clwb:       %b\n%!" clwb ;

  let flags = [] in
  let flags = if clflush then "-DART_CLFLUSH" :: flags else flags in
  let flags = if clflushopt then "-DART_CLFLUSHOPT" :: flags else flags in
  let flags = if clwb then "-DART_CLWB" :: flags else flags in
  Configurator.V1.Flags.write_sexp "flush.sexp" flags
