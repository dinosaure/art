open Rresult

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let insert _ path (key : Rowex.key) value =
  let th0 =
    let open Part in
    let* () = open_index writer ~path:(Fpath.to_string path) in
    let* () = insert key value in
    close in
  match Part.(run closed th0) with
  | _closed, () -> `Ok 0
  | exception Rowex.Duplicate ->
    `Error (false, Fmt.str "%S already exists into %a." (key :> string) Fpath.pp path)

open Cmdliner

let setup_logs style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (reporter Fmt.stderr) ;
  Option.is_none level

let common_options = "COMMON OPTIONS"

let verbosity =
  let env = Arg.env_var "PART_LOGS" in
  Logs_cli.level ~docs:common_options ~env ()

let renderer =
  let env = Arg.env_var "PART_FMT" in
  Fmt_cli.style_renderer ~docs:common_options ~env ()

let setup_logs = Term.(const setup_logs $ renderer $ verbosity)

let existing_file =
  let parser str = match Fpath.of_string str with
    | Ok _ as v when Sys.file_exists str 
                  && Sys.file_exists (str ^ ".socket") -> v
    | Ok v -> R.error_msgf "%a (or its socket) does not exist." Fpath.pp v
    | Error _ as err -> err in
  Arg.conv (parser, Fpath.pp)

let key =
  let parser str =
    try R.ok (Rowex.key str)
    with _ -> R.error_msgf "Invalid key: %S" str in
  let pp : Rowex.key Fmt.t = fun ppf key -> Fmt.string ppf (key :> string) in
  Arg.conv (parser, pp)

let file =
  let doc = "Path of the index file." in
  Arg.(required & pos 0 (some existing_file) None & info [] ~doc)

let key =
  let doc = "The key to insert into the given index file." in
  Arg.(required & pos 1 (some key) None & info [] ~doc)

let value =
  let doc = "The value associated to the key." in
  Arg.(required & pos 2 (some int) None & info [] ~doc)

let cmd =
  let doc = "A simple executable to insert an occurence into a index file with a specific value." in
  let man =
    [ `S Manpage.s_description
    ; `P "$(tname) inserts a value associated with the given key into the given index." ] in
  Term.(ret (const insert $ setup_logs $ file $ key $ value)),
  Term.info "insert" ~doc ~man

let () = Term.(exit_status @@ eval cmd)
