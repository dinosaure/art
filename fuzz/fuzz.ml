open Crowbar

let const x _ = x

let key = map [ bytes ] @@ fun k ->
  try let k = Art.key k in k
  with Invalid_argument _ -> bad_test ()

let add = map [ key; int ] @@ fun k v -> `Add (k, v)
let lookup = map [ key ] @@ fun k -> `Lookup k
let action = choose [ lookup; add; ]
let test = list1 action

let failf fmt = Fmt.kstrf fail fmt
type action = [ `Add of Art.key * int | `Lookup of Art.key ]

let pp : action Fmt.t = fun ppf v -> match v with
  | `Add (k, n) -> Fmt.pf ppf "(`Add (%S, %d))" (k :> string) n
  | `Lookup k -> Fmt.pf ppf "(`Lookup %S)" (k :> string)

(* XXX(dinosaure): it's not an effective test but let me waste CPU clock. *)

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

let pp_binding ppf ((k : Art.key), v) =
  Fmt.pf ppf "@[<1>(%S,@ %d)@]" (k :> string) v

(* XXX(dinosaure): this test is more appropriate. *)

let () =
  add_test ~name:"art" [ list (pair key int) ] @@ fun lst ->
  let art = Art.make () in
  List.iter (fun (k, v) -> Art.insert art k v) lst ;
  let check = fun (k, v0) -> match Art.find art k with
    | v1 -> check_eq v0 v1
    | exception Not_found -> failf "Error with: @[<hov>%a@]" Fmt.(Dump.list pp_binding) lst in
  List.iter check lst
