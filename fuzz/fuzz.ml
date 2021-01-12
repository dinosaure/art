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
  let check = fun (k, v0) -> Art.insert art k v0 ; match Art.find art k with
    | v1 -> check_eq v0 v1
    | exception Not_found -> failf "Error with: @[<hov>%a@]" Fmt.(Dump.list pp_binding) lst in
  List.iter check lst

let unique equal lst =
  let rec go k acc = function
    | [] -> acc
    | (k', _) as hd :: tl -> if equal k k' then go k acc tl else go k' (hd :: acc) tl in
  match List.rev lst with
  | [] -> []
  | (k, v) :: lst ->
     go k [ k, v ] lst

let () =
  add_test ~name:"art" [ list (pair key int) ] @@ fun lst ->
  let art = Art.make () in
  List.iter (fun (k, v) -> Art.insert art k v) lst ;
  let uniq = List.stable_sort (fun ((a : Art.key), _) ((b : Art.key), _) -> String.compare (a:>string) (b:>string)) lst in
  let uniq = unique (fun (a:Art.key) (b:Art.key) -> String.equal (a:>string) (b:>string)) uniq in
  let check = fun (k, v0) -> match Art.find art k with
    | v1 -> check_eq v0 v1
    | exception Not_found -> failf "Error with: @[<hov>%a@]" Fmt.(Dump.list pp_binding) lst in
  List.iter check uniq

(* XXX(dinosaure): we can use [String.compare] but I would like to
 * ensure ordering on tests. *)
let string_compare a b =
  let idx = ref 0 in
  let res = ref 0 in
  while !idx < String.length a && !idx < String.length b &&
        ( res := Char.code a.[!idx] - Char.code b.[!idx]
        ; !res = 0 )
  do incr idx done ;
  if !res = 0 && String.length a = String.length b then 0
  else if !res = 0
  then ( if !idx = String.length a then 0 - Char.code b.[!idx]
         else Char.code a.[!idx] )
  else !res

let () =
  add_test ~name:"art/minimum" [ list1 (pair key int) ] @@ fun lst ->
  let art = Art.make () in
  List.iter (fun (k, v) -> Art.insert art k v) lst ;
  let uniq = List.stable_sort (fun ((a : Art.key), _) ((b : Art.key), _) -> string_compare (a:>string) (b:>string)) lst in
  let uniq = unique (fun (a:Art.key) (b:Art.key) -> String.equal (a:>string) (b:>string)) uniq in
  let (k0, v0) = List.hd uniq in
  let (k1, v1) = Art.minimum art in
  check_eq ~pp:(fun ppf (v:Art.key) -> Fmt.pf ppf "%S" (v :> string)) k0 k1 ;
  check_eq ~pp:Fmt.int v0 v1

let () =
  add_test ~name:"art/remove" [ list1 (pair key int); list1 (pair key int) ] @@ fun l0 l1 ->
  let tree = Art.make () in
  List.iter (fun (k, v) -> Art.insert tree k v) l0 ;
  List.iter (fun (k, v) -> Art.insert tree k v) l1 ;
  List.iter (fun (k, _) -> Art.remove tree k) l1 ;
  let check = fun (k, v0) -> match List.assoc k l1 with
    | _ -> ()
    | exception Not_found ->
      let v1 = Art.find tree k in
      check_eq v0 v1 in
  let l0 = List.stable_sort (fun ((a : Art.key), _) ((b : Art.key), _) -> String.compare (a:>string) (b:>string)) l0 in
  let l0 = unique (fun (a:Art.key) (b:Art.key) -> String.equal (a:>string) (b:>string)) l0 in
  List.iter check l0
