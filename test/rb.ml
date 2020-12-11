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

open Rowex
open Persistent
open Rresult

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
  let len = Ringbuffer.size_of_order order in
  let fd  = Unix.openfile filename Unix.[ O_CREAT; O_RDWR ] 0o644 in
  let _   = Unix.lseek fd len Unix.SEEK_SET in
  let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.int Bigarray.c_layout true [| len |] in
  let memory = Bigarray.array1_of_genarray memory in
  let memory = to_memory memory in
  fd, memory

let test_spsc ~(order:Ringbuffer.order) ?(len= Random.int (1 lsl (order :> int))) filename =
  let lst = List.init len
    (fun _ -> 1 + Random.int ((1 lsl (order :> int)) - 1)) in
  let eoq = 0 in
  let fiber0 () =
    let fd, ring = load ~order filename in
    let rec go fd acc =
      Unix.fsync fd ;
      let res = rrun ring (Ringbuffer.dequeue ~order ~non_empty:true (Addr.of_int_rdwr 0)) in
      Fmt.epr "[%a] dequeue %d.\n%!" Fmt.(styled `Blue (fmt "%10d")) (Unix.getpid ()) res ; 
      if res = eoq
      then
        ( Fmt.epr "[%a] done.\n%!" Fmt.(styled `Blue (fmt "%10d")) (Unix.getpid ())
        ; List.rev acc )
      else go fd (res :: acc) in
    let res = go fd [] in Unix.close fd ; res in
  let fiber1 () =
    let fd, ring = load ~order filename in
    let rec go fd lst = match lst with
      | [] ->
        rrun ring (Ringbuffer.enqueue ~order ~non_empty:false (Addr.of_int_rdwr 0) eoq) ;
        Unix.fsync fd ;
        Fmt.epr "[%a] done.\n%!" Fmt.(styled `Blue (fmt "%10d")) (Unix.getpid ()) ; Unix.close fd
      | hd :: tl ->
        rrun ring (Ringbuffer.enqueue ~order ~non_empty:false (Addr.of_int_rdwr 0) hd) ;
        Unix.fsync fd ; go fd tl in
    Unix.fsync fd ; go fd lst in
  let open Fiber in
  fork_and_join (fun () -> run_process fiber0) (fun () -> run_process fiber1) >>= fun ress ->
  Fmt.epr "Processes done.\n%!" ;
  match ress with
  | Ok lst', Ok () ->
    if lst = lst'
    then return (R.ok ())
    else ( Fmt.epr "%s: Failure.\n%!" Sys.argv.(0)
         ; Fmt.epr "- : @[<hov>%a@].\n%!" Fmt.(Dump.list int) lst
         ; Fmt.epr "- : @[<hov>%a@].\n%!" Fmt.(Dump.list int) lst'
         ; return (R.error_msgf "Failure") )
  | Error exit, _ ->
    return (R.error_msgf "Reader exits with %03d" exit)
  | _, Error exit ->
    return (R.error_msgf "Writer exits with %03d" exit)

let main order len () () =
  let open Bos in
  OS.File.tmp "ring-%s" >>= fun path ->
  Logs.debug (fun m -> m "Ring file: %a" Fpath.pp path) ;
  create ~order (Fpath.to_string path) ;
  Fiber.run (test_spsc ~order ?len (Fpath.to_string path))

open Cmdliner

let setup_logs style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (reporter Fmt.stderr)

let setup_logs =
  Term.(const setup_logs $ Fmt_cli.style_renderer () $ Logs_cli.level ())

external random_seed : unit -> int array = "caml_sys_random_seed"

let seed = "4EygbdYh+v35vvrmD9YYP4byT5E3H7lTeXJiIj+dQnc="
let seed = Base64.decode_exn seed

let seed =
  let res = Array.make (String.length seed / 2) 0 in
  for i = 0 to (String.length seed / 2) - 1 do
    res.(i) <- (Char.code seed.[i * 2] lsl 8) lor Char.code seed.[(i * 2) + 1]
  done;
  res

let setup_random = function
  | true ->
    let random_seed = seed in
    Fmt.pr "Random: %a.\n%!" Fmt.(Dump.array int) random_seed;
    Random.full_init random_seed
  | false -> Random.self_init ()

let predictable ?env () =
  let doc = Fmt.str "Use a static seed to initiate the random generator." in
  Arg.(value & flag & info ~doc ?env [ "predictable" ])

let setup_random =
  Term.(const setup_random $ predictable ())

let order =
  let parser x = match int_of_string x with
    | x -> Ok (Ringbuffer.order_of_int x)
    | exception _ -> R.error_msgf "Invalid order: %S" x in
  let pp ppf (order:Ringbuffer.order) = Fmt.int ppf (order :> int) in
  Arg.conv (parser, pp)

let order = Arg.(required & pos 0 (some order) None & info [])
let length = Arg.(value & pos 1 (some int) None & info [])

let main =
  Term.(term_result (const main $ order $ length $ setup_random $ setup_logs)),
  Term.info "ring"

let () = Term.(exit @@ eval main)
