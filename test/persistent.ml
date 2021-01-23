let () = Printexc.record_backtrace true

external random_seed : unit -> int array = "caml_sys_random_seed"

let seed = "4EygbdYh+v36vvrmD9YYP4byT5E3H7lTeXJiIj+dQnc="
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

let identity x = x
let empty = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0

let create filename =
  let len = 1_048_576 in
  let fd  = Unix.openfile filename Unix.[ O_CREAT; O_RDWR ] 0o644 in
  let _   = Unix.lseek fd len Unix.SEEK_SET in
  let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.int Bigarray.c_layout true [| len |] in
  let memory = Bigarray.array1_of_genarray memory in
  let memory = to_memory memory in
  let brk = size_of_word * 2 in
  atomic_set_leuintnat memory 0 Seq_cst brk ;
  let mmu = mmu_of_memory ~sync:identity () ~ring:empty memory in
  let root = run mmu (Persistent.ctor ()) in
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
  let mmu = mmu_of_memory ~sync:identity () ~ring:empty memory in
  Unix.close fd ; mmu

let random_index =
  Lazy.from_fun @@ fun () ->
  let open Rresult in
  Bos.OS.File.tmp "index-%s" >>| fun index ->
  create (Fpath.to_string index) ; mmu_of_file (Fpath.to_string index)

let mmu_of_optional_file = function
  | Some mmu -> mmu
  | None -> Rresult.R.failwith_error_msg (Lazy.force random_index)

let insert mmu root key v = run mmu (Persistent.insert root (Rowex.key key) v)
let find mmu root key = run mmu (Persistent.find root (Rowex.key key))

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a][%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Blue (fmt "%10d")) (Unix.getpid ())
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let setup_logs style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (reporter Fmt.stderr)

let () = setup_logs (Some `Ansi_tty) (Some Logs.Debug)

let test01 =
  Alcotest.test_case "test01" `Quick @@ fun file ->
  let mmu = mmu_of_optional_file file in
  let root = atomic_get_leuintnat (memory_of_mmu mmu) size_of_word Seq_cst in
  let root_rdwr = Addr.of_int_rdwr root in
  let root_rd   = Addr.of_int_rdonly root in
  insert mmu root_rdwr "abc"   1 ;
  Alcotest.(check int) "abc"   (find mmu root_rd "abc")   1 ;
  insert mmu root_rdwr "ab"    2 ;
  Alcotest.(check int) "abc"   (find mmu root_rd "abc")   1 ;
  Alcotest.(check int) "ab"    (find mmu root_rd "ab")    2 ;
  insert mmu root_rdwr "abcde" 3 ;
  Alcotest.(check int) "abc"   (find mmu root_rd "abc")   1 ;
  Alcotest.(check int) "ab"    (find mmu root_rd "ab")    2 ;
  Alcotest.(check int) "abcde" (find mmu root_rd "abcde") 3
;;

let test02 =
  Alcotest.test_case "test02" `Quick @@ fun file ->
  let mmu = mmu_of_optional_file file in
  let root = atomic_get_leuintnat (memory_of_mmu mmu) size_of_word Seq_cst in
  let root_rdwr = Addr.of_int_rdwr root in
  let root_rd   = Addr.of_int_rdonly root in
  insert mmu root_rdwr "a0" 0 ;
  insert mmu root_rdwr "a1" 1 ;
  insert mmu root_rdwr "a2" 2 ;
  insert mmu root_rdwr "a3" 3 ;
  Alcotest.(check int) "a0" (find mmu root_rd "a0") 0 ;
  Alcotest.(check int) "a1" (find mmu root_rd "a1") 1 ;
  Alcotest.(check int) "a2" (find mmu root_rd "a2") 2 ;
  Alcotest.(check int) "a3" (find mmu root_rd "a3") 3 ;
  insert mmu root_rdwr "a4" 4 ;
  Alcotest.(check int) "a0" (find mmu root_rd "a0") 0 ;
  Alcotest.(check int) "a1" (find mmu root_rd "a1") 1 ;
  Alcotest.(check int) "a2" (find mmu root_rd "a2") 2 ;
  Alcotest.(check int) "a3" (find mmu root_rd "a3") 3 ;
  Alcotest.(check int) "a4" (find mmu root_rd "a4") 4
;;

let random_string len =
  let res = Bytes.create len in
  for i = 0 to len - 1 do Bytes.set res i (Char.chr (1 + Random.int 255)) done ;
  Bytes.unsafe_to_string res

let test03 =
  Alcotest.test_case "test03" `Quick @@ fun file ->
  let max = 500 in
  let mmu = mmu_of_optional_file file in
  let root = atomic_get_leuintnat (memory_of_mmu mmu) size_of_word Seq_cst in
  let root_rdwr = Addr.of_int_rdwr root in
  let root_rd   = Addr.of_int_rdonly root in
  let vs = List.init max (fun _ -> random_string (1 + Random.int 63), Random.int max) in
  List.iter (fun (k, v) -> insert mmu root_rdwr k v) vs ;
  Alcotest.(check pass) "insertion" () () ;
  let check k v =
    Fmt.pr ">>> find %S.\n%!" k ;
    let v' = find mmu root_rd k in
    Fmt.pr ">>> found %d.\n%!" v' ;
    Alcotest.(check int) (Fmt.str "%S" k) v' v in
  List.iter (fun (k, v) -> check k v) vs
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
    [ "simple", [ test01; test02; test03 ] ]
