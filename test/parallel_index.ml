let () = Printexc.record_backtrace true

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

open Rresult

let identity x = x
let size_of_word = Sys.word_size / 8

let exists state v key =
  match Part.(run state (find (Rowex.unsafe_key key))) with
  | state, v' ->
    if v <> v'
    then ( Logs.err (fun m -> m "Wrong value for %s." (key :> string)) ; exit 1 )
    else ( Logs.debug (fun m -> m "%s => %d." (key :> string) v) ; true, state )
  | exception Not_found -> false, state

type kind = [ `Simple_consumer_simple_producer
            | `Multiple_consumer_simple_producer ]

let test ~kind dataset path =
  Logs.debug (fun m -> m "Create the index.") ;
  let fiber0 () =
    let th0 =
      let open Part in
      let rec go ic n = match input_line ic with
        | line ->
          Logs.debug (fun m -> m "Insert %S." line) ;
          let* () = Part.insert (Rowex.unsafe_key line) n in
          go ic (succ n)
        | exception End_of_file ->
          Logs.debug (fun m -> m "End of writer") ;
          close_in ic ; close in
      Logs.debug (fun m -> m "Start the writer.") ;
      let* () = open_index writer ~path in
      go (open_in (Fpath.to_string dataset)) 0 in
    let _closed, () = Part.(run closed th0) in () in
  let fiber1 dataset () =
    let rec go state dataset queue =
      let ress = Array.make (Array.length dataset) false in
      let fold (idx, state) key =
        let res, state = exists state idx key in 
        ress.(idx) <- res ;
        succ idx, state in
      let _, state  = Array.fold_left fold (0, state) dataset in
      if not (Array.for_all identity ress)
      then ( let _missing =
               let fold acc = function
                 | true -> acc
                 | false -> succ acc in
               Array.fold_left fold 0 ress in
             Logs.debug (fun m -> m "Missing %d elements." _missing)
           ; Queue.push ress queue
           ; go state dataset queue )
      else
        ( Logs.debug (fun m -> m "End of reader: @[<hov>%a@]"
            Fmt.(Dump.array bool) ress)
        ; Queue.push ress queue
        ; state, Queue.fold (fun ress x -> x :: ress) [] queue ) in
    Logs.debug (fun m -> m "Start the reader.") ;
    let uid = Unix.getpid () in
    let uid = Int64.of_int uid in
    let state, () = Part.(run closed (open_index (reader uid) ~path)) in
    let state, resss = go state dataset (Queue.create ()) in
    let _closed, () = Part.(run state close) in
    resss in
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
        let temp = R.failwith_error_msg (Tmp.tmp "fiber-%s") in
        Logs.debug (fun m -> m "Run one reader.\n%!") ;
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
    let temp = R.failwith_error_msg (Tmp.tmp "fiber-%s") in
    ( fork_and_join
        (fun () -> run_process fiber0)
        (fun () -> run_process ~file:(Fpath.to_string temp) (fiber1 dataset))
      >>= function
      | Ok (), Ok histogram ->
        Fmt.pr ">>> %d iteration(s).\n%!" (List.length histogram) ;
        return (Ok ())
      | Error exit, _ ->
        return (R.error_msgf "Reader exits with %03d" exit)
      | _, Error exit ->
        return (R.error_msgf "Writer exits with %03d" exit) )

let main multiple_readers dataset () () () =
  Tmp.tmp "index-%s" >>= fun path ->
  Logs.debug (fun m -> m "Index file: %a" Fpath.pp path) ;
  let kind = match multiple_readers with
    | true  -> `Multiple_consumer_simple_producer
    | false -> `Simple_consumer_simple_producer in
  let path = Fpath.to_string path in
  match Part.(run closed (create path)) with
  | _closed, Ok () -> Fiber.run (test ~kind dataset path)
  | _closed, (Error _ as err) -> err

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

let setup_tmp = function
  | Some path ->
    let _ = R.failwith_error_msg (Bos.OS.Dir.create ~path:true path) in
    Tmp.set_default_dir path
  | None -> ()

let fpath = Arg.conv (Fpath.of_string, Fpath.pp)

let setup_tmp =
  Term.(const setup_tmp $ Arg.(value & opt (some fpath) None & info [ "tmp" ]))

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
  Cmd.v
    (Cmd.info "ring")
  Term.(term_result (const main $ multiple_readers $ dataset $ setup_concurrency $ setup_tmp $ setup_logs))

let () = Cmd.(exit @@ eval main)
