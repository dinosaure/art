open Crowbar

let key = map [ bytes ] @@ fun k ->
  if k = "" then bad_test () ;
  try let k = Rowex.key k in k
  with Invalid_argument _ -> bad_test ()

let memory = Bytes.create 0xFFFFFFF
module Mem = Mem.Make (struct let memory = memory end)
module Art = Rowex.Make (Mem) 

let () =
  add_test ~name:"simple" [ list (pair key int) ] @@ fun lst ->
  let root = Art.make () in
  List.fold_left (fun acc (k, v) -> match Art.insert root k v with
    | () -> (k, v) :: acc
    | exception Out_of_memory -> bad_test ()
    | exception Rowex.Duplicate -> acc) [] lst
  |> List.iter @@ fun (k, v) -> match Art.find root k with
  | v' -> check_eq v v'
  | exception Not_found -> failf "%S not found" (k :> string)
