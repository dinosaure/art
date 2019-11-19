open Core_bench

let insert_hashtbl keys =
  let tbl = Hashtbl.create (Array.length keys) in
  Bench.Test.create ~name:"insert hashtbl" @@ fun () ->
  for i = 0 to Array.length keys - 1
  do Hashtbl.add tbl keys.(i) i done

let insert_art keys =
  let art = Art.make () in
  Bench.Test.create ~name:"insert art" @@ fun () ->
  for i = 0 to Array.length keys - 1
  do Art.insert art keys.(i) i done

let () =
  let keys = Array.make (int_of_string Sys.argv.(1)) "" in
  let rec populate v = match input_line stdin with
    | hash -> keys.(v) <- hash ; populate (succ v)
    | exception End_of_file -> () in
  populate 0 ; Bench.bench [ insert_hashtbl keys; insert_art keys; ]


