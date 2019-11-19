open Crowbar

let add = map [ bytes; int ] @@ fun k v -> `Add (k, v)
let lookup = map [ bytes ] @@ fun k -> `Lookup k
let action = choose [ lookup; add; ]
let test = list1 action

let failf fmt = Fmt.kstrf fail fmt

let pp ppf = function
  | `Add (k, n) -> Fmt.pf ppf "(`Add (%S, %d))" k n
  | `Lookup k -> Fmt.pf ppf "(`Lookup %S)" k

let () =
  add_test ~name:"art" [ test ] @@ fun tests ->
  let tbl = Hashtbl.create (List.length tests) in
  let art = Art.make () in
  List.iter (function
      | `Add (k, v) ->
        Hashtbl.add tbl k v ;
        Art.insert art k v
      | `Lookup k ->
        match Hashtbl.find tbl k, Art.find art k with
        | v0, v1 -> check_eq v0 v1
        | exception Not_found ->
          if Hashtbl.mem tbl k
          then failf "Error with: @[<hov>%a@]" Fmt.(Dump.list pp) tests)
    tests
