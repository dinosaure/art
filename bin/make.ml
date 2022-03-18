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

let make _ path =
  let th0 =
    let open Part in
    create (Fpath.to_string path) in
  match Part.(run closed th0) with
  | _closed, Ok () -> `Ok ()
  | _closed, Error (`Msg err) ->
    `Error (false, Fmt.str "%s." err)
  | exception exn ->
    Logs.err (fun m -> m "Got an error while creating %a: %s"
      Fpath.pp path (Printexc.to_string exn)) ;
    `Error (false, Fmt.str "Got an error while creating %a."
      Fpath.pp path)

open Cmdliner

let setup_logs style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (reporter Fmt.stderr) ;
  Option.is_none level

let common_options = "COMMON OPTIONS"

let verbosity =
  let env = Cmd.Env.info "PART_LOGS" in
  Logs_cli.level ~docs:common_options ~env ()

let renderer =
  let env = Cmd.Env.info "PART_FMT" in
  Fmt_cli.style_renderer ~docs:common_options ~env ()

let setup_logs = Term.(const setup_logs $ renderer $ verbosity)

let non_existing_file =
  let parser str = match Fpath.of_string str with
    | Ok _ as v when not (Sys.file_exists str)
                  && not (Sys.file_exists (str ^ ".socket")) -> v
    | Ok v -> R.error_msgf "The index (and its socket) %a already exists." Fpath.pp v
    | Error _ as err -> err in
  Arg.conv (parser, Fpath.pp)

let file =
  let doc = "The name of the index." in
  Arg.(required & pos 0 (some non_existing_file) None & info [] ~doc)

let cmd =
  let doc = "A simple executable to create a new index." in
  let man =
    [ `S Manpage.s_description
    ; `P "$(tname) creates a new index." ] in
  Cmd.v
    (Cmd.info "make" ~doc ~man)
    Term.(ret (const make $ setup_logs $ file))

let () = Cmd.(exit @@ eval cmd)
