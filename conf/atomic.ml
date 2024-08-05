let parse s = Scanf.sscanf s "%d.%d" (fun major minor -> (major, minor))

let () =
  let version = parse Sys.ocaml_version in
  if version >= (4, 12) && version < (5, 0) then print_string "atomic_stdlib.ml"
  else print_string "atomic_pre412.ml"
