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

let identity x = x
let size_of_word = Sys.word_size / 8

module RB = struct
  let order = Ringbuffer.order

  let create filename =
    let filename = Fpath.to_string filename in
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

  let load filename =
    let filename = Fpath.to_string filename in
    let len = Ringbuffer.size_of_order order in
    let fd  = Unix.openfile filename Unix.[ O_CREAT; O_RDWR ] 0o644 in
    let _   = Unix.lseek fd len Unix.SEEK_SET in
    let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.int Bigarray.c_layout true [| len |] in
    let memory = Bigarray.array1_of_genarray memory in
    let memory = to_memory memory in
    fd, memory
end

module Index = struct
  let empty = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0

  let create filename =
    let len = 1_048_576 in
    let filename = Fpath.to_string filename in
    let fd  = Unix.openfile filename Unix.[ O_CREAT; O_RDWR ] 0o644 in
    let _   = Unix.lseek fd len Unix.SEEK_SET in
    let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.int Bigarray.c_layout true [| len |] in
    let memory = Bigarray.array1_of_genarray memory in
    let memory = to_memory memory in
    let brk = size_of_word * 2 in
    atomic_set_leuintnat memory 0 Seq_cst brk ;
    let mmu = mmu_of_memory ~sync:identity () ~ring:empty memory in
    let root = run mmu (Rowex.ctor ()) in
    atomic_set_leuintnat memory (Sys.word_size / 8) Seq_cst (root :> int) ;
    Unix.close fd
  ;;

  let page_size = 4096

  let wr_mmu_of_file (fd_ring, ring) filename =
    let filename = Fpath.to_string filename in
    let fd = Unix.openfile filename Unix.[ O_RDWR ] 0o644 in
    let len = ((Unix.fstat fd).st_size + page_size) / page_size in (* XXX(dinosaure): padding. *)
    let len = len * page_size in
    let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.char Bigarray.c_layout true [| len |] in
    let memory = Bigarray.array1_of_genarray memory in
    let mmu = mmu_of_memory ~sync:Unix.fsync fd_ring ~ring memory in
    Unix.close fd ; mmu

  let append_reader fd_ring ring =
    let zero = Addr.of_int_rdwr 0 in
    Unix.fsync fd_ring ; rrun ring Ringbuffer.(enqueue ~order ~non_empty:false zero (Unix.getpid ())) ; Unix.fsync fd_ring

  let delete_reader fd_ring ring = append_reader fd_ring ring

  let rd_mmu_of_file (fd_ring, ring) filename =
    let filename = Fpath.to_string filename in
    let fd = Unix.openfile filename Unix.[ O_RDWR ] 0o644 in
    let len = ((Unix.fstat fd).st_size + page_size) / page_size in (* XXX(dinosaure): padding. *)
    let len = len * page_size in
    let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.char Bigarray.c_layout true [| len |] in
    let memory = Bigarray.array1_of_genarray memory in
    let mmu = mmu_of_memory ~sync:identity () ~ring:empty memory in
    Unix.close fd ; append_reader fd_ring ring ; mmu
end

let insert mmu root key v = run mmu (Rowex.insert root (Rowex.unsafe_key key) v)
let lookup mmu root key = run mmu (Rowex.find root (Rowex.unsafe_key key))

let exists mmu root key =
  match lookup mmu root key with
  | _v -> true
  | exception Not_found -> false

type kind = [ `Simple_consumer_simple_producer
            | `Multiple_consumer_simple_producer ]

let test ~kind dataset filename =
  let ring_filename = Fpath.add_ext "ring" filename in
  RB.create ring_filename ;
  Index.create filename ;
  let fiber0 () =
    let fd_ring, ring = RB.load ring_filename in
    let mmu = Index.wr_mmu_of_file (fd_ring, ring) filename in
    let root = atomic_get_leuintnat (memory_of_mmu mmu) size_of_word Seq_cst in
    let root_rdwr = Addr.of_int_rdwr root in
    let rec go ic n = match input_line ic with
      | line -> insert mmu root_rdwr line n ; go ic (succ n)
      | exception End_of_file ->
        Logs.debug (fun m -> m "End of writer") ;
        close_in ic in
    go (open_in (Fpath.to_string dataset)) 0 in
  let fiber1 dataset () =
    let fd_ring, ring = RB.load ring_filename in
    let mmu = Index.rd_mmu_of_file (fd_ring, ring) filename in
    let root = atomic_get_leuintnat (memory_of_mmu mmu) size_of_word Seq_cst in
    let root_rd = Addr.of_int_rdonly root in
    let rec go dataset queue =
      let res = Array.map (exists mmu root_rd) dataset in
      if not (Array.for_all identity res)
      then ( Queue.push res queue ; go dataset queue )
      else
        ( Index.delete_reader fd_ring ring
        ; Logs.debug (fun m -> m "End of reader: @[<hov>%a@]" Fmt.(Dump.array bool) res)
        ; Queue.push res queue
        ; Queue.fold (fun res x -> x :: res) [] queue ) in
    go dataset (Queue.create ()) in
  let dataset =
    let rec go ic acc = match input_line ic with
      | line -> go ic (line :: acc)
      | exception End_of_file ->
        close_in ic ; Array.of_list (List.rev acc) in
    go (open_in (Fpath.to_string dataset)) [] in
  match kind with
  | `Multiple_consumer_simple_producer ->
    let open Fiber in
    let readers () =
      let f _ =
        let temp = R.failwith_error_msg (Bos.OS.File.tmp "fiber-%s") in
        run_process ~file:(Fpath.to_string temp) (fiber1 dataset) >>= fun _res ->
        return () in
      parallel_iter ~f (List.init (get_concurrency () - 1) identity) >>= fun () ->
      return (Ok ()) in
    let writer () = run_process fiber0 in
    ( fork_and_join writer readers >>= function
    | Ok (), Ok () ->
      Fmt.pr ">>> %d reader(s) terminate correctly.\n%!" (get_concurrency () - 1) ;
      return (Ok ())
    | Error exit, _ ->
      return (R.error_msgf "Reader exits with %03d" exit)
    | _, Error exit ->
      return (R.error_msgf "Writer exits with %03d" exit) )
  | `Simple_consumer_simple_producer ->
    let open Fiber in
    let temp = R.failwith_error_msg (Bos.OS.File.tmp "fiber-%s") in
    ( fork_and_join (fun () -> run_process fiber0) (fun () -> run_process ~file:(Fpath.to_string temp) (fiber1 dataset)) >>= function
    | Ok (), Ok histogram ->
      Fmt.pr ">>> %d iteration(s).\n%!" (List.length histogram) ;
      return (Ok ())
    | Error exit, _ ->
      return (R.error_msgf "Reader exits with %03d" exit)
    | _, Error exit ->
      return (R.error_msgf "Writer exits with %03d" exit) )

let main multiple_readers dataset () () =
  let open Bos in
  OS.File.tmp "index-%s" >>= fun path ->
  Logs.debug (fun m -> m "Index file: %a" Fpath.pp path) ;
  let kind = match multiple_readers with
    | true  -> `Multiple_consumer_simple_producer
    | false -> `Simple_consumer_simple_producer in
  Fiber.run (test ~kind dataset path)

open Cmdliner

let setup_logs style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (reporter Fmt.stderr)

let setup_logs =
  Term.(const setup_logs $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let setup_concurrency v =
  Fiber.set_concurrency v

let setup_concurrency =
  Term.(const setup_concurrency $ Arg.(value & opt int (Fiber.get_concurrency ()) & info [ "c"; "concurrency" ]))

let existing_file =
  let parser x = match Fpath.of_string x with
    | Ok _ as v when Sys.file_exists x -> v
    | Ok v -> R.error_msgf "%a not found" Fpath.pp v
    | Error _ as err -> err in
  Arg.conv (parser, Fpath.pp)

let dataset =
  Arg.(required & opt (some existing_file) None & info [ "dataset" ])

let multiple_readers =
  Arg.(value & flag & info [ "multiple-readers" ])

let main =
  Term.(term_result (const main $ multiple_readers $ dataset $ setup_concurrency $ setup_logs)),
  Term.info "ring"

let () = Term.(exit @@ eval main)
