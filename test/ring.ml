external random_seed : unit -> int array = "caml_sys_random_seed"

let seed = "4EygbdYh+v35vvrmD9YYP4byT5E3H7lTeXJiIj+dQnc="
let seed = Base64.decode_exn seed

let seed =
  let res = Array.make (String.length seed / 2) 0 in
  for i = 0 to (String.length seed / 2) - 1 do
    res.(i) <- (Char.code seed.[i * 2] lsl 8) lor Char.code seed.[(i * 2) + 1]
  done;
  res

let () =
  let random_seed = seed in
  Fmt.pr "Random: %a.\n%!" Fmt.(Dump.array int) random_seed;
  Random.full_init random_seed

open Rowex
open Persistent

let size_of_word = Sys.word_size / 8

let create ~order filename =
  let len = Ringbuffer.size_of_order order in
  let fd  = Unix.openfile filename Unix.[ O_CREAT; O_RDWR ] 0o644 in
  let _   = Unix.lseek fd len Unix.SEEK_SET in
  let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.int Bigarray.c_layout true [| len |] in
  let memory = Bigarray.array1_of_genarray memory in
  let memory = to_memory memory in
  atomic_set_leuintnat memory (size_of_word * 0) Seq_cst 0 ;
  atomic_set_leuintnat memory (size_of_word * 1) Seq_cst 0 ;
  atomic_set_leuintnat memory (size_of_word * 2) Seq_cst (-1) ;
  for i = 0 to (1 lsl ((order :> int) + 1)) - 1 do
    atomic_set_leuintnat memory (size_of_word * (3 + i)) Seq_cst (-1)
  done ;
  Unix.close fd

let load ~order filename =
  let filename = Fpath.to_string filename in
  let len = Ringbuffer.size_of_order order in
  let fd  = Unix.openfile filename Unix.[ O_CREAT; O_RDWR ] 0o644 in
  let _   = Unix.lseek fd len Unix.SEEK_SET in
  let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.int Bigarray.c_layout true [| len |] in
  let memory = Bigarray.array1_of_genarray memory in
  let memory = to_memory memory in
  fd, memory

let order = Ringbuffer.order_of_int 15
let zero = Addr.of_int_rdwr 0

let random_filename = Lazy.from_fun @@ fun () ->
  let filename = Rresult.R.failwith_error_msg (Bos.OS.File.tmp "ring-%s") in
  create ~order (Fpath.to_string filename) ; filename

let load ~order = function
  | Some filename -> load ~order filename
  | None -> load ~order (Lazy.force random_filename)

let push ring v = rrun ring Ringbuffer.(enqueue ~order ~non_empty:false zero v)
let pop ?(non_empty= false) ring = rrun ring Ringbuffer.(dequeue ~order ~non_empty zero)
let peek ?(non_empty= false) ring = rrun ring Ringbuffer.(peek ~order ~non_empty zero)

let test01 =
  Alcotest.test_case "test01" `Quick @@ fun filename ->
  let fd, ring = load ~order filename in
  push ring 1 ;
  push ring 2 ;
  push ring 3 ;
  push ring 4 ;
  Alcotest.(check int) "1" (pop ring) 1 ;
  Alcotest.(check int) "2" (pop ring) 2 ;
  Alcotest.(check int) "3" (pop ring) 3 ;
  Alcotest.(check int) "4" (pop ring) 4 ;
  Unix.close fd
;;

let test02 =
  Alcotest.test_case "test02" `Quick @@ fun filename ->
  let fd, ring = load ~order filename in
  push ring 1 ;
  Alcotest.(check int) "1" (pop ring) 1 ;
  Alcotest.(check int) "null" (pop ring) (lnot 0) ;
  push ring 2 ;
  Alcotest.(check int) "2" (pop ring) 2 ;
  Alcotest.(check int) "null" (pop ring) (lnot 0) ;
  Unix.close fd
;;

let test03 =
  Alcotest.test_case "test03" `Quick @@ fun filename ->
  let fd, ring = load ~order filename in
  push ring 1 ;
  Alcotest.(check int) "1" (peek ring) 1 ;
  push ring 2 ;
  Alcotest.(check int) "1" (peek ring) 1 ;
  push ring 3 ;
  Alcotest.(check int) "1" (peek ring) 1 ;
  Alcotest.(check int) "1" (pop ring) 1 ;
  Alcotest.(check int) "2" (peek ring) 2 ;
  Alcotest.(check int) "2" (pop ring) 2 ;
  Alcotest.(check int) "3" (peek ring) 3 ;
  Alcotest.(check int) "3" (pop ring) 3 ;
  Alcotest.(check int) "null" (peek ring) (lnot 0) ;
  Unix.close fd
;;

open Cmdliner

let filename =
  let parser x = match Fpath.of_string x with
    | Ok v when not (Sys.file_exists x) ->
      create ~order (Fpath.to_string v) ; Ok v
    | Ok v -> Rresult.R.error_msgf "%a already exists" Fpath.pp v
    | Error _ as err -> err in
  let pp ppf _ = Fmt.pf ppf "#ring" in
  Arg.conv (parser, pp)

let filename =
  let doc = "The persistent ring file." in
  Arg.(value & opt (some filename) None & info [ "ring" ] ~doc)

let () = Alcotest.run_with_args "ring" filename
           [ "simple", [ test01; test02; test03 ] ]
