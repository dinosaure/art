open Crowbar

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a][%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Blue (fmt "%10d")) (Unix.getpid ())
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let setup_logs style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (reporter Fmt.stderr)

let () = setup_logs (Some `Ansi_tty) (Some Logs.Debug)

let path = "index.idx"

let key = map [ bytes ] @@ fun k ->
  if k = "" then bad_test () ;
  try let k = Rowex.key k in k
  with Invalid_argument _ -> bad_test ()

let memory = Bytes.create 0xFFFFFF
module Mem = Mem.Make (struct let memory = memory end)
module Art = Rowex.Make (Mem) 

let () =
  add_test ~name:"simple" [ list (pair key int) ] @@ fun lst ->
  let root = Art.make () in
  let pp = Art.formatter ~commit:ignore Fmt.stdout in
  List.fold_left (fun acc (k, v) -> match Art.insert root k v with
    | () ->
        (Art.pp pp root) ;
        (k, v) :: acc
    | exception Out_of_memory -> bad_test ()
    | exception Rowex.Duplicate -> acc) [] lst
  |> List.iter @@ fun (k, v) -> match Art.find root k with
  | v' -> check_eq v v'
  | exception Not_found -> failf "%S not found" (k :> string)
