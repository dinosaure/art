let () = Printexc.record_backtrace true

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

let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stdout)
let () = Logs.set_level ~all:true (Some Logs.Debug)

type memory = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let identity x = x
let empty = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0

external get_page_size
  : unit -> int
  = "caml_getpagesize" [@@noalloc]

let page_size = get_page_size ()

let mmu_of_file filename = Part.unsafe_mmu_of_file filename

let read_eval_print_loop mmu =
  let rec loop mmu =
    Format.printf ": %!" ;
    match Astring.String.cuts ~sep:" " (input_line stdin) with
    | [ "insert"; key; value; ] ->
      ( try let value = int_of_string value in
            Part.insert mmu key value
        with exn ->
          Format.printf "# %s.\n%!" (Printexc.to_string exn) ;
          Printexc.print_backtrace stdout ;
          Format.printf "> Invalid key or value: %S -> %S.\n%!" key value ;
          loop mmu )
    | [ "lookup"; key; ] ->
      ( try let value = Part.lookup mmu key in
          Format.printf "> %S => %10d.\n%!" key (value :> int) ;
          loop mmu
        with
        | Not_found ->
           Format.printf "> %S does not exists.\n%!" key ;
           loop mmu
        | Invalid_argument _ ->
           Format.printf "> Invalid key %S.\n%!" key ;
           loop mmu )
    | [ "quit" ] -> ()
    | vs ->
      Format.printf "> Invalid command: %S.\n%!" (String.concat " " vs) ;
      loop mmu
    | exception End_of_file -> () in
  loop mmu

let size_of_word = Sys.word_size / 8

let create filename len = Part.create ~len filename

(* XXX(dinosaure): hard-part, see [coreutils/dd.c] or [xstrtoumax]. *)
let size_of_string str =
  let open Astring in
  let m, n = match String.head ~rev:true str with
    | Some 'K' | Some 'k' -> 1_024, 1
    | Some 'M' | Some 'm' -> 1_048_576, 1_024
    | _ -> 1, 0 in
  match String.cut ~sep:"." str with
  | Some (a, b) ->
    let a = int_of_string a in
    let b = int_of_string (String.take ~sat:Astring.Char.Ascii.is_digit b) in
    a * m + (b * n)
  | None ->
    let a = int_of_string (String.take ~sat:Astring.Char.Ascii.is_digit str) in
    a * m

let () = match Sys.argv with
  | [| _; filename; |] when Sys.file_exists filename ->
    let ipc = Fmt.strf "%s.socket" filename in
    let ipc =
      if Sys.file_exists ipc then Ipc.connect ipc
      else ( Rresult.R.failwith_error_msg (Ipc.create ipc) ; Ipc.connect ipc ) in
    let mmu = mmu_of_file ipc filename in
    read_eval_print_loop mmu
  | [| _; "create"; len; filename |] ->
    let len = size_of_string len in
    let _ = create filename len in ()
  | _ -> Format.eprintf "%s [create <len>] <filename>\n%!" Sys.argv.(0)
