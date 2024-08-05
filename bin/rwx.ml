let () = Printexc.record_backtrace true

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt
  in
  { Logs.report }

let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stdout)
let () = Logs.set_level ~all:true (Some Logs.Debug)

let insert state key value =
  let th0 =
    let open Part in
    insert key value
  in
  match Part.run state th0 with
  | state, Ok () -> state
  | state, Error `Already_exists ->
      Fmt.pr "# %S already exists.\n%!" (key :> string);
      state

let lookup state key =
  let th0 =
    let open Part in
    find key
  in
  match Part.run state th0 with
  | state, value ->
      Fmt.pr "> %S => %10d.\n%!" (key :> string) value;
      state
  | exception Not_found ->
      Fmt.pr "> %S does not exists.\n%!" (key :> string);
      state

let parse_key key = try Ok (Rowex.key key) with _ -> Error `Invalid_key

let parse_value value =
  try Ok (int_of_string value) with _ -> Error `Invalid_value

let read_eval_print_loop path =
  let rec loop state =
    Fmt.pr ": %!";
    match Astring.String.cuts ~sep:" " (input_line stdin) with
    | [ "insert"; key; value ] -> (
        match (parse_key key, parse_value value) with
        | Ok key, Ok value ->
            let state = insert state key value in
            loop state
        | Error _, _ ->
            Fmt.pr "# Invalid key %S.\n%!" key;
            loop state
        | _, Error _ ->
            Fmt.pr "# Invalid value %S.\n%!" value;
            loop state)
    | [ "lookup"; key ] -> (
        match parse_key key with
        | Ok key -> lookup state key |> loop
        | Error _ ->
            Fmt.pr "# Invalid key %S.\n%!" key;
            loop state)
    | [ "quit" ] -> Part.run state Part.close
    | vs ->
        Fmt.pr "> Invalid command: %S.\n%!" (String.concat " " vs);
        loop state
    | exception End_of_file -> Part.run state Part.close
  in
  let th0 = Part.(open_index writer ~path) in
  let state, () = Part.(run closed th0) in
  let _closed = loop state in
  ()

let create path len =
  let th0 =
    let open Part in
    create ~len path
  in
  match Part.(run closed th0) with
  | _closed, Ok () -> ()
  | _closed, Error (`Msg err) -> Fmt.epr "%s." err

(* XXX(dinosaure): hard-part, see [coreutils/dd.c] or [xstrtoumax]. *)
let size_of_string str =
  let open Astring in
  let m, n =
    match String.head ~rev:true str with
    | Some 'K' | Some 'k' -> (1_024, 1)
    | Some 'M' | Some 'm' -> (1_048_576, 1_024)
    | _ -> (1, 0)
  in
  match String.cut ~sep:"." str with
  | Some (a, b) ->
      let a = int_of_string a in
      let b = int_of_string (String.take ~sat:Astring.Char.Ascii.is_digit b) in
      (a * m) + (b * n)
  | None ->
      let a =
        int_of_string (String.take ~sat:Astring.Char.Ascii.is_digit str)
      in
      a * m

let () =
  match Sys.argv with
  | [| _; path |] when Sys.file_exists path -> read_eval_print_loop path
  | [| _; "create"; len; path |] ->
      let len = size_of_string len in
      create path len
  | _ -> Format.eprintf "%s [create <len>] <filename>\n%!" Sys.argv.(0)
