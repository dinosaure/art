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

(*
let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stdout)
let () = Logs.set_level ~all:true (Some Logs.Debug)
*)

open Crowbar

let key =
  map [ bytes ] @@ fun k ->
  if k = "" then bad_test ();
  try
    let k = Rowex.key k in
    k
  with Invalid_argument _ -> bad_test ()

let memory = Bytes.make 0xfffffff '\000'

module Mem = Mem.Make (struct
  let memory = memory
end)

module Art = Rowex.Make (Mem)

let () =
  add_test ~name:"simple" [ list (pair key int) ] @@ fun lst ->
  let tree = Art.make () in
  List.fold_left
    (fun acc (k, v) ->
      match Art.insert tree k v with
      | () -> (k, v) :: acc
      | exception Out_of_memory -> bad_test ()
      | exception Rowex.Duplicate -> acc)
    [] lst
  |> List.iter @@ fun (k, v) ->
     match Art.find tree k with
     | v' -> check_eq v v'
     | exception Not_found -> failf "%S not found" (k :> string)

let unique equal lst =
  let rec go k acc = function
    | [] -> acc
    | ((k', _) as hd) :: tl ->
        if equal k k' then go k acc tl else go k' (hd :: acc) tl
  in
  match List.rev lst with [] -> [] | (k, v) :: lst -> go k [ (k, v) ] lst

let () =
  add_test ~name:"remove" [ list1 (pair key int); list1 (pair key int) ]
  @@ fun l0 l1 ->
  let tree = Art.make () in
  let l0 =
    List.fold_left
      (fun acc (k, v) ->
        match Art.insert tree k v with
        | () -> (k, v) :: acc
        | exception Out_of_memory -> bad_test ()
        | exception Rowex.Duplicate -> acc)
      [] l0
  in
  let l1 =
    List.fold_left
      (fun acc (k, v) ->
        match Art.insert tree k v with
        | () -> (k, v) :: acc
        | exception Out_of_memory -> bad_test ()
        | exception Rowex.Duplicate -> acc)
      [] l1
  in
  List.iter (fun (k, _) -> try Art.remove tree k with Not_found -> ()) l1;
  let check (k, v0) =
    match List.assoc k l1 with
    | _ -> ()
    | exception Not_found ->
        let v1 = Art.find tree k in
        check_eq v0 v1
  in
  let l0 =
    List.stable_sort
      (fun ((a : Rowex.key), _) ((b : Rowex.key), _) ->
        String.compare (a :> string) (b :> string))
      l0
  in
  let l0 =
    unique
      (fun (a : Rowex.key) (b : Rowex.key) ->
        String.equal (a :> string) (b :> string))
      l0
  in
  List.iter check l0
