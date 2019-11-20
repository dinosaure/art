open Crowbar

let add = map [ bytes; int ] @@ fun k v ->
  try let k = Art.key k in `Add (k, v)
  with Invalid_argument _ -> bad_test ()
let lookup = map [ bytes ] @@ fun k ->
  try let k = Art.key k in `Lookup k
  with Invalid_argument _ -> bad_test ()
let action = choose [ lookup; add; ]
let test = list1 action

let failf fmt = Fmt.kstrf fail fmt
type action = [ `Add of Art.key * int | `Lookup of Art.key ]

let pp : action Fmt.t = fun ppf v -> match v with
  | `Add (k, n) -> Fmt.pf ppf "(`Add (%S, %d))" (k :> string) n
  | `Lookup k -> Fmt.pf ppf "(`Lookup %S)" (k :> string)

let () =
  add_test ~name:"art" [ test ] @@ fun (tests : action list) ->
  let tbl = Hashtbl.create (List.length tests) in
  let art = Art.make () in
  List.iter (function
      | `Add (k, v) ->
        Art.insert art k v ;
        Hashtbl.add tbl (k :> string) v
      | `Lookup k ->
        match Art.find art k, Hashtbl.find tbl (k :> string) with
        | v0, v1 -> check_eq v0 v1
        | exception Not_found ->
          if Hashtbl.mem tbl (k :> string)
          then failf "Error with: @[<hov>%a@]" Fmt.(Dump.list pp) tests)
    tests
