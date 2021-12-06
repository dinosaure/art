let src = Logs.Src.create "fiber"
module Log = (val Logs.src_log src : Logs.LOG)

let () = Printexc.record_backtrace true

type 'a t = ('a -> unit) -> unit

let return x k = k x

let ( >>> ) a b k = a (fun () -> b k)

let ( >>= ) t f k = t (fun x -> f x k)

let ( >>| ) t f k = t (fun x -> k (f x))

let both a b =
  a >>= fun a ->
  b >>= fun b -> return (a, b)

module Ivar = struct
  type 'a state = Full of 'a | Empty of ('a -> unit) Queue.t

  type 'a t = { mutable state : 'a state }

  let create () = { state = Empty (Queue.create ()) }

  let fill t x =
    match t.state with
    | Full _ -> failwith "Ivar.fill"
    | Empty q ->
        t.state <- Full x ;
        Queue.iter (fun f -> f x) q

  let read t k = match t.state with Full x -> k x | Empty q -> Queue.push k q
end

type 'a ivar = 'a Ivar.t

module Future = struct
  let wait = Ivar.read
end

let fork f k =
  let ivar = Ivar.create () in
  f () (fun x -> Ivar.fill ivar x) ;
  k ivar

let fork_and_join f g =
  fork f >>= fun a ->
  fork g >>= fun b -> both (Future.wait a) (Future.wait b)

let fork_and_join_unit f g =
  fork f >>= fun a ->
  fork g >>= fun b -> Future.wait a >>> Future.wait b

let rec parallel_map l ~f =
  match l with
  | [] -> return []
  | x :: l ->
      fork (fun () -> f x) >>= fun future ->
      parallel_map l ~f >>= fun l ->
      Future.wait future >>= fun x -> return (x :: l)

let rec parallel_iter l ~f =
  match l with
  | [] -> return ()
  | x :: l ->
      fork (fun () -> f x) >>= fun future ->
      parallel_iter l ~f >>= fun () -> Future.wait future

let safe_close fd = try Unix.close fd with _exn -> ()

let _reporter pid ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("[%06d]%a[%a]: " ^^ fmt ^^ "\n%!")
        pid Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let create_process ?file prgn =
  let out0, out1 = match file with
    | None -> Unix.pipe ()
    | Some filename ->
      Log.debug (fun m -> m "Save result of children into %s." filename) ;
      let ic = Unix.openfile filename Unix.[ O_RDONLY; O_CREAT; O_TRUNC ] 0o644 in
      let oc = Unix.openfile filename Unix.[ O_WRONLY; O_CREAT; O_TRUNC ] 0o644 in
      ic, oc in
  match Unix.fork () with
  | 0 -> (
      Unix.close out0 ;
      let oc = Unix.out_channel_of_descr out1 in
      try
        let res = prgn () in
        Log.debug (fun m -> m "End of the process %d." (Unix.getpid ())) ;
        Marshal.to_channel oc res [ Marshal.No_sharing ] ;
        Log.debug (fun m -> m "Result of %d marshalled." (Unix.getpid ())) ;
        flush oc ; close_out oc ;
        exit 0
      with exn ->
        Log.err (fun m -> m "Got an error: %S" (Printexc.to_string exn)) ;
        Log.err (fun m -> m "Backtrace: %s" (Printexc.get_backtrace ())) ;
        exit 127)
  | pid ->
      Unix.close out1 ;
      (out0, pid)

let get_concurrency () =
  try
    let ic = Unix.open_process_in "getconf _NPROCESSORS_ONLN" in
    let close () = ignore (Unix.close_process_in ic) in
    let sc = Scanf.Scanning.from_channel ic in
    try Scanf.bscanf sc "%d" (fun n -> close () ; n)
    with exn -> close () ; raise exn
  with
  | Not_found | Sys_error _ | Failure _ | Scanf.Scan_failure _
  | End_of_file | Unix.Unix_error (_, _, _) -> 1

let concurrency = ref (get_concurrency ())

let running = Hashtbl.create ~random:false !concurrency

let waiting_for_slot = Queue.create ()

let set_concurrency n = concurrency := n

let get_concurrency () = !concurrency

let throttle () =
  if Hashtbl.length running >= !concurrency
  then (
    let ivar = Ivar.create () in
    Queue.push ivar waiting_for_slot ;
    Ivar.read ivar)
  else return ()

let restart_throttle () =
  while
    Hashtbl.length running < !concurrency
    && not (Queue.is_empty waiting_for_slot)
  do
    Ivar.fill (Queue.pop waiting_for_slot) ()
  done

let signals =
  [ Sys.sigabrt, "SIGABRT"
  ; Sys.sigalrm, "SIGALRM"
  ; Sys.sigfpe,  "SIGFPE"
  ; Sys.sighup,  "SIGHUP"
  ; Sys.sigill,  "SIGILL"
  ; Sys.sigint,  "SIGINT"
  ; Sys.sigkill, "SIGKILL"
  ; Sys.sigpipe, "SIGPIPIE"
  ; Sys.sigquit, "SIGQUIT"
  ; Sys.sigsegv, "SIGSEGV"
  ; Sys.sigterm, "SIGTERM"
  ; Sys.sigusr1, "SIGUSR1"
  ; Sys.sigusr2, "SIGUSR2"
  ; Sys.sigchld, "SIGCHLD"
  ; Sys.sigcont, "SIGCONT"
  ; Sys.sigstop, "SIGSTOP"
  ; Sys.sigtstp, "SIGTSTP"
  ; Sys.sigttin, "SIGTTIN"
  ; Sys.sigttou, "SIGTTOU"
  ; Sys.sigvtalrm, "SIGVTALRM"
  ; Sys.sigprof, "SIGPROF"
  ; Sys.sigbus,  "SIGBUS"
  ; Sys.sigpoll, "SIGPOLL"
  ; Sys.sigsys,  "SIGSYS"
  ; Sys.sigtrap, "SIGTRAP"
  ; Sys.sigurg,  "SIGURG"
  ; Sys.sigxcpu, "SIGXCPU"
  ; Sys.sigxfsz, "SIGXFSZ" ]

let pp_signal ppf signal = match List.assoc_opt signal signals with
  | Some str -> Format.fprintf ppf "%s" str
  | None -> Format.fprintf ppf "%d" signal

let run_process ?file prgn =
  throttle () >>= fun () ->
  let fd, pid = create_process ?file prgn in
  let ivar = Ivar.create () in
  Hashtbl.add running pid ivar ;
  Ivar.read ivar >>= fun status ->
  let ic = Unix.in_channel_of_descr fd in
  match status with
  | Unix.WEXITED 0 ->
    let res = Marshal.from_channel ic in
    safe_close fd ;
    return (Ok res)
  | Unix.WEXITED n ->
    safe_close fd ;
    return (Error n)
  | Unix.WSIGNALED signal ->
    Log.warn (fun m -> m "The processus %6d terminated with a signal: %a." pid pp_signal signal) ; 
    safe_close fd ;
    return (Error 255)
  | Unix.WSTOPPED _ -> assert false

let run fiber =
  let result = ref None in
  fiber (fun x -> result := Some x) ;
  let rec loop () =
    if Hashtbl.length running > 0
    then (
      let pid, status = Unix.wait () in
      let ivar = Hashtbl.find running pid in
      Hashtbl.remove running pid ;
      Ivar.fill ivar status ;
      restart_throttle () ;
      loop ())
    else match !result with Some x -> x | None -> failwith "fiber" in
  loop ()
