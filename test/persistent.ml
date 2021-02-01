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

open Persistent

let identity x = x
let empty = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0

let create filename = Part.create filename

let page_size = 4096

let mmu_of_file filename = Part.unsafe_mmu_of_file filename

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
  Part.insert mmu "abc"   1 ;
  Alcotest.(check int) "abc"   (Part.lookup mmu "abc")   1 ;
  Part.insert mmu "ab"    2 ;
  Alcotest.(check int) "abc"   (Part.lookup mmu "abc")   1 ;
  Alcotest.(check int) "ab"    (Part.lookup mmu "ab")    2 ;
  Part.insert mmu "abcde" 3 ;
  Alcotest.(check int) "abc"   (Part.lookup mmu "abc")   1 ;
  Alcotest.(check int) "ab"    (Part.lookup mmu "ab")    2 ;
  Alcotest.(check int) "abcde" (Part.lookup mmu "abcde") 3
;;

let test02 =
  Alcotest.test_case "test02" `Quick @@ fun file ->
  let mmu = mmu_of_optional_file file in
  Part.insert mmu "a0" 0 ;
  Part.insert mmu "a1" 1 ;
  Part.insert mmu "a2" 2 ;
  Part.insert mmu "a3" 3 ;
  Alcotest.(check int) "a0" (Part.lookup mmu "a0") 0 ;
  Alcotest.(check int) "a1" (Part.lookup mmu "a1") 1 ;
  Alcotest.(check int) "a2" (Part.lookup mmu "a2") 2 ;
  Alcotest.(check int) "a3" (Part.lookup mmu "a3") 3 ;
  Part.insert mmu "a4" 4 ;
  Alcotest.(check int) "a0" (Part.lookup mmu "a0") 0 ;
  Alcotest.(check int) "a1" (Part.lookup mmu "a1") 1 ;
  Alcotest.(check int) "a2" (Part.lookup mmu "a2") 2 ;
  Alcotest.(check int) "a3" (Part.lookup mmu "a3") 3 ;
  Alcotest.(check int) "a4" (Part.lookup mmu "a4") 4
;;

let random_string len =
  let res = Bytes.create len in
  for i = 0 to len - 1 do Bytes.set res i (Char.chr (1 + Random.int 255)) done ;
  Bytes.unsafe_to_string res

let test03 =
  Alcotest.test_case "test03" `Quick @@ fun file ->
  let max = 500 in
  let mmu = mmu_of_optional_file file in
  let vs = List.init max (fun _ -> random_string (1 + Random.int 63), Random.int max) in
  List.iter (fun (k, v) -> Part.insert mmu k v) vs ;
  Alcotest.(check pass) "insertion" () () ;
  let check k v =
    let v' = Part.lookup mmu k in
    Alcotest.(check int) (Fmt.str "%S" k) v' v in
  List.iter (fun (k, v) -> check k v) vs
;;

let test04 =
  Alcotest.test_case "test04" `Quick @@ fun file ->
  let mmu = mmu_of_optional_file file in
  Part.insert mmu "stone@meekness.com" 0 ;
  Part.insert mmu "ca-tech@dps,centrin.net.id" 1 ;
  Part.insert mmu "trinanda_lestyowati@elkomsel.co.id" 2 ;
  Part.insert mmu "asst_dos@asonrasuna.com" 3 ;
  Part.insert mmu "amartabali@dps.centrim.net.id" 4 ;
  Part.insert mmu "achatv@cbn.net.id" 5 ;
  Part.insert mmu "bali@tuguhotels.com" 6 ;
  Part.insert mmu "baliminimalist@yahoo.com" 7 ; (* prefix with [li] on a n16 node *)
  Alcotest.(check int) "bali@tuguhotels.com" (Part.lookup mmu "bali@tuguhotels.com") 6 ;
  Alcotest.(check int) "baliminimalist@yahoo.com" (Part.lookup mmu "baliminimalist@yahoo.com") 7 ;
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
    [ "simple", [ test01; test02; test03; test04 ] ]
