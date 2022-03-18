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

type fmt = Fmt : string * (int -> unit, Format.formatter, unit) format -> fmt

let show (Fmt (_, fmt)) v = Fmt.pr fmt v

let find _ fmt path (key : Rowex.key) =
  let uid = Unix.getpid () in
  let uid = Int64.of_int uid in
  let th0 =
    let open Part in
    let* () = open_index (reader uid) ~path:(Fpath.to_string path) in
    let* result = find key in
    let* () = close in
    return result in
  match Part.(run closed th0) with
  | _closed, value -> show fmt value ; `Ok ()
  | exception Not_found ->
    `Error (false, Fmt.str "%S does not exists." (key :> string))

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

let fmt =
  let parser str =
    try R.ok (Fmt (str, Scanf.format_from_string str "%d"))
    with Scanf.Scan_failure _ -> R.error_msgf "Invalid format: %S" str in
  let pp ppf (Fmt (str, _)) = Fmt.string ppf str in
  Arg.conv (parser, pp)

let existing_file =
  let parser str = match Fpath.of_string str with
    | Ok _ as v when Sys.file_exists str -> v
    | Ok v -> R.error_msgf "%a does not exist." Fpath.pp v
    | Error _ as err -> err in
  Arg.conv (parser, Fpath.pp)

let key =
  let parser str =
    try R.ok (Rowex.key str)
    with _ -> R.error_msgf "Invalid key: %S" str in
  let pp : Rowex.key Fmt.t = fun ppf key -> Fmt.string ppf (key :> string) in
  Arg.conv (parser, pp)

let fmt =
  let doc = "How to format the output <integer>." in
  Arg.(value & opt fmt (Fmt ("%d", "%d" ^^ "")) & info [ "fmt" ] ~doc)

let file =
  let doc = "Path of the index file." in
  Arg.(required & pos 0 (some existing_file) None & info [] ~doc)

let key =
  let doc = "The key to search into the given index file." in
  Arg.(required & pos 1 (some key) None & info [] ~doc)

let cmd =
  let doc = "A simple executable to search an occurence into a index file." in
  let man =
    [ `S Manpage.s_description
    ; `P "$(tname) searches for the value associated with the given key." ] in
  Cmd.v
    (Cmd.info "find" ~doc ~man)
    Term.(ret (const find $ setup_logs $ fmt $ file $ key))

let () = Cmd.(exit @@ eval cmd)
