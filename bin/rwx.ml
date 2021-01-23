let () = Printexc.record_backtrace true

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stdout)
let () = Logs.set_level ~all:true (Some Logs.Debug)

open Rowex
open Persistent

type memory = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let identity x = x
let empty = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0

external get_page_size
  : unit -> int
  = "caml_getpagesize" [@@noalloc]

let page_size = get_page_size ()

let mmu_of_file filename =
  let fd = Unix.openfile filename Unix.[ O_RDWR ] 0o644 in
  let len = ((Unix.fstat fd).st_size + page_size) / page_size in (* XXX(dinosaure): padding. *)
  let len = len * page_size in
  let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.char Bigarray.c_layout true [| len |] in
  let memory = Bigarray.array1_of_genarray memory in
  let mmu = mmu_of_memory ~sync:identity () ~ring:empty memory in
  Unix.close fd ; mmu

let read_eval_print_loop mmu root =
  let rec loop mmu root =
    Format.printf ": %!" ;
    match Astring.String.cuts ~sep:" " (input_line stdin) with
    | [ "insert"; key; value; ] ->
      ( try let value = int_of_string value in run mmu (Persistent.insert root (Rowex.key key) value) ; loop mmu root
        with exn ->
          Format.printf "# %s.\n%!" (Printexc.to_string exn) ;
          Printexc.print_backtrace stdout ;
          Format.printf "> Invalid key or value: %S -> %S.\n%!" key value ;
          loop mmu root )
    | [ "lookup"; key; ] ->
      ( try let value = run mmu (Persistent.find (Addr.to_rdonly root) (Rowex.key key)) in
          Format.printf "> %S => %10d.\n%!" key (value :> int) ;
          loop mmu root
        with
        | Not_found ->
           Format.printf "> %S does not exists.\n%!" key ;
           loop mmu root
        | Invalid_argument _ ->
           Format.printf "> Invalid key %S.\n%!" key ;
           loop mmu root )
    | [ "quit" ] -> ()
    | vs ->
      Format.printf "> Invalid command: %S.\n%!" (String.concat " " vs) ;
      loop mmu root
    | exception End_of_file -> () in
  loop mmu root

let size_of_word = Sys.word_size / 8

let create filename len =
  let fd = Unix.openfile filename Unix.[ O_CREAT; O_RDWR ] 0o644 in
  let _  = Unix.lseek fd len Unix.SEEK_SET in
  let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.char Bigarray.c_layout true [| len |] in
  let memory = Bigarray.array1_of_genarray memory in
  let brk = size_of_word * 2 in
  Logs.debug (fun m -> m "[brk]  set %016x %016x" 0 brk) ;
  atomic_set_leuintnat memory 0 Seq_cst brk ;
  let mmu = mmu_of_memory ~sync:identity () ~ring:empty memory in
  let root = run mmu (Persistent.ctor ()) in
  Logs.debug (fun m -> m "[root] set %016x %016x" size_of_word (root :> int)) ;
  atomic_set_leuintnat memory (Sys.word_size / 8) Seq_cst (root :> int) ;
  Unix.close fd
;;

(* XXX(dinosaure): hard-part, see [coreutils/dd.c] or [xstrtoumax]. *)
let size_of_string str =
  let open Astring in
  let m, n = match String.head ~rev:true str with
    | Some 'K' | Some 'k' -> 1_024, 1
    | Some 'M' | Some 'm' -> 1_048_576, 1_024
    | _ -> 1, 0 in
  match String.cut ~sep:"." str with
  | Some (a, b) ->
    let a = int_of_string a in
    let b = int_of_string (String.take ~sat:Astring.Char.Ascii.is_digit b) in
    a * m + (b * n)
  | None ->
    let a = int_of_string (String.take ~sat:Astring.Char.Ascii.is_digit str) in
    a * m

let () = match Sys.argv with
  | [| _; filename; |] when Sys.file_exists filename ->
    let mmu = mmu_of_file filename in
    let root = atomic_get_leuintnat (memory_of_mmu mmu) size_of_word Seq_cst in
    read_eval_print_loop mmu (Addr.of_int_rdwr root)
  | [| _; "create"; len; filename |] ->
    let len = size_of_string len in
    create filename len
  | _ -> Format.eprintf "%s [create <len>] <filename>\n%!" Sys.argv.(0)
