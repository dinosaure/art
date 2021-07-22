open Monolith
open PPrint

module Map = Map.Make (struct type t = Art.key let compare (a : Art.key) (b : Art.key) =
  String.compare (a :> string) (b :> string) end)

let char_without_d0 () = match Gen.char () with
  | '\000' -> Gen.reject ()
  | chr -> chr

let key =
  easily_constructible
    Gen.(fun () -> Art.unsafe_key (string (int 100) char_without_d0 ()))
    (fun (key : Art.key) -> string (key :> string))

let value = lt 16

module type INDEX = sig
  type t

  val make : unit -> t
  val insert : t -> Art.key -> int -> unit
  val find_opt : t -> Art.key -> int option
  val is_empty : t -> bool
end

module Make (R : INDEX) (C : INDEX) = struct
  let run t fuel =
    declare "make" (unit ^> t) R.make C.make;
    declare "is_empty" (t ^> bool) R.is_empty C.is_empty;
    declare "insert" (t ^> key ^> value ^> unit) R.insert C.insert;
    declare "find_opt" (t ^> key ^> option value) R.find_opt C.find_opt;
    main fuel
end

module Reference = struct
  type t = (Art.key, int) Hashtbl.t

  let make () = Hashtbl.create 0x100
  let is_empty tbl = Hashtbl.length tbl = 0
  let insert tbl key value = Hashtbl.add tbl key value
  let find_opt tbl key = match Hashtbl.find tbl key with
    | v -> Some v
    | exception Not_found -> None
end

module Equivalence = Make (Reference) (struct include Art type t = int Art.t end)

let () =
  let t = declare_abstract_type () in
  Equivalence.run t 5
