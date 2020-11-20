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

let size_of_word = Sys.word_size / 8

open Rowex
open Persistent

let create filename =
  let len = 1_048_576 in
  let fd  = Unix.openfile filename Unix.[ O_CREAT; O_RDWR ] 0o644 in
  let _   = Unix.lseek fd len Unix.SEEK_SET in
  let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.char Bigarray.c_layout true [| len |] in
  let memory = Bigarray.array1_of_genarray memory in
  let brk = size_of_word * 2 in
  atomic_set_leuintnat memory 0 Seq_cst brk ;
  let mmu = mmu_of_memory memory in
  let root = run mmu (Rowex.ctor ()) in
  atomic_set_leuintnat memory (Sys.word_size / 8) Seq_cst (root :> int) ;
  Unix.close fd
;;

let page_size = 4096

let mmu_of_file filename =
  let fd = Unix.openfile filename Unix.[ O_RDWR ] 0o644 in
  let len = ((Unix.fstat fd).st_size + page_size) / page_size in (* XXX(dinosaure): padding. *)
  let len = len * page_size in
  let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.char Bigarray.c_layout true [| len |] in
  let memory = Bigarray.array1_of_genarray memory in
  let mmu = mmu_of_memory memory in
  Unix.close fd ; mmu 

let random_index =
  Lazy.from_fun @@ fun () ->
  let open Rresult in
  Bos.OS.File.tmp "index-%s" >>| fun index ->
  create (Fpath.to_string index) ; mmu_of_file (Fpath.to_string index)

let mmu_of_optional_file = function
  | Some mmu -> mmu
  | None -> Rresult.R.failwith_error_msg (Lazy.force random_index)

let test01 =
  Alcotest.test_case "test01" `Quick @@ fun file ->
  let mmu = mmu_of_optional_file file in
  let root = atomic_get_leuintnat (memory_of_mmu mmu) size_of_word Seq_cst in
  let root = Addr.of_int_rdwr root in
  run mmu (Rowex.insert root "abc" 1) ;
  Alcotest.(check int) "abc"   (run mmu (Rowex.find (Addr.to_rdonly root) "abc"))   1 ;
  run mmu (Rowex.insert root "ab" 2) ;
  Alcotest.(check int) "abc"   (run mmu (Rowex.find (Addr.to_rdonly root) "abc"))   1 ;
  Alcotest.(check int) "ab"    (run mmu (Rowex.find (Addr.to_rdonly root) "ab"))    2 ;
  run mmu (Rowex.insert root "abcde" 3) ;
  Alcotest.(check int) "abc"   (run mmu (Rowex.find (Addr.to_rdonly root) "abc"))   1 ;
  Alcotest.(check int) "ab"    (run mmu (Rowex.find (Addr.to_rdonly root) "ab"))    2 ;
  Alcotest.(check int) "abcde" (run mmu (Rowex.find (Addr.to_rdonly root) "abcde")) 3
;;

open Cmdliner

let filename =
  let parser x = match Fpath.of_string x with
    | Ok v when not (Sys.file_exists x) -> create (Fpath.to_string v) ; Ok (mmu_of_file (Fpath.to_string v))
    | Ok v -> Rresult.R.error_msgf "%a already exists" Fpath.pp v
    | Error _ as err -> err in
  let pp ppf _ = Fmt.pf ppf "#index" in
  Arg.conv (parser, pp)

let filename =
  let doc = "The persistent index file." in
  Arg.(value & opt (some filename) None & info [ "index" ] ~doc)

let () = Alcotest.run_with_args "rowex" filename
           [ "simple", [ test01 ] ]
