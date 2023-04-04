open Crowbar

let path = "index.idx"

let key = map [ bytes ] @@ fun k ->
  if k = "" then bad_test () ;
  try let k = Rowex.key k in k
  with Invalid_argument _ -> bad_test ()

let () =
  let open Part in
  match create path |> run closed with
  | _, Ok () -> ()
  | _, Error (`Msg msg) -> failf "Got an error when we created the IDX file: %s" msg

let () =
  add_test ~name:"simple" [ list (pair key int) ] @@ fun lst ->
  let insert_all lst =
    let open Part in
    let rec go tbl = function
      | [] -> return tbl
      | (k, v) :: r ->
        let* () = Part.insert k v in
        Hashtbl.replace tbl k v;
        go tbl r in
    let* () = open_index Part.writer ~path in
    let* tbl = go (Hashtbl.create 0x10) lst in
    let lst = Hashtbl.fold (fun k v a -> (k, v) :: a) tbl [] in
    let* () = close in return lst in
  let find_all lst =
    let open Part in
    let rec go acc = function
      | [] -> return acc
      | (k, v) :: r ->
        let* v' = Part.find k in
        check_eq v v' ; go (acc && (v = v')) r in
    let* () = open_index (Part.reader 0L) ~path in
    let* res = go true lst in
    let* ()  = close in
    return res in
  let state, lst = Part.(run closed) (insert_all lst) in
  check_eq (Part.is_closed state) true;
  let state, res = Part.(run closed) (find_all lst) in
  check_eq (Part.is_closed state) true;
  check_eq res true
