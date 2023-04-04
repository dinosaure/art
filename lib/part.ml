open Rowex

let src = Logs.Src.create "part"
module Log = (val Logs.src_log src : Logs.LOG)

external msync : Persistent.memory -> unit = "part_msync" [@@noalloc]

type 'c capabilities =
  | Reader : int64 -> ro capabilities
  | Writer : rdwr capabilities

type ('fd, 'c) fd =
  | Truncate_and_file_descr : Ipc.t * Unix.file_descr -> (Unix.file_descr, rdwr) fd
  | Truncate : Ipc.t -> (none, ro) fd
and none = |

let reader uid = Reader uid
let writer = Writer

type 'c opened = | constraint 'c = < .. >
type closed = |

type 'v state =
  | Opened : 'c Persistent.mmu * 'c capabilities * ('fd, 'c) fd -> 'c opened state
  | Closed : closed state

let closed = Closed

type ('p, 'q, 'a) t =
  | Return : 'a -> ('p, 'p, 'a) t
  | Bind : ('p, 'q, 'a) t * ('a -> ('q, 'r, 'b) t) -> ('p, 'r, 'b) t
  | Open : 'c capabilities * string -> (closed, 'c opened, unit) t
  | Create : string * int -> (closed, closed, (unit, [> `Msg of string ]) result) t
  | Close : ('c opened, closed, unit) t
  | Find : key -> ('c rd opened, 'c rd opened, int) t
  | Insert : key * int -> (rdwr opened, rdwr opened, unit) t

let return x = Return x
let open_index c ~path = Open (c, path)
let find key = Find key
let insert key value = Insert (key, value)
let close = Close
let bind x f = Bind (x, f)
let create ?(len= 1048576) path = Create (path, len)
let ( let* ) = bind


let is_closed : type v. v state -> bool = function
  | Closed -> true
  | Opened _ -> false

(* XXX(dinosaure): an explanation is needed between [truncate] and [remap].
 * [truncate] updates the size of the index file and invalidate any current
 * readers then about their [memory] values.
 *
 * To solve that, we use a signal from the writer to ask to reader to [remap]
 * their [memory] according to the following [Unix.LargeFile.ftruncate]. To
 * proceed:
 * + [Persistent] gives to us alive readers
 * + the writer acquire the lock on the [ipc] (so a new reader can **not**
 *   appear)
 * + the writer send a signal to readers
 * + readers are already configured to receive the signal - see [Open] with
 *   [Reader]
 * + [remap] on readers is called and interrupt anything
 * + a reader will send to the writer (via [trc]) that is ready to [remap]
 * + a reader wants to acquire the lock on [ipc]
 * + the writer waiting all readers (and consume [trc])
 * + it applies [msync] (to ensure that everything is done)
 * + it truncates the file
 * + it reloads the memory with the new size and return the new address
 *   to [Persistent]
 * + it releases the lock on [ipc]
 * + readers will start a battle to acquire first the lock on [ipc]
 * + one reader [remap] the file truncated
 * + this reader will unsafely set its memory address to the new one
 * + and it releases the lock to let other readers to do the same
 *
 * TODO(dinosaure): signal is bad
 * TODO(dinosaure): OCaml does not tell to us who sent the signal but it help
 * us to implement a ping-pong mechanism between writer and readers instead of
 * a new FIFO [%s-truncate.socket].
 * TODO(dinosaure): by design, [msync] is probably not needed
 *
 * NOTE(dinosaure): [remap] does not do padding (as [Open]) because it
 * assumes that [Open] does.
 *)

let page_size = 4096 (* TODO(dinosaure): replace it by a call. *)

let signal_readers readers =
  Persistent.Hashset.iter (fun reader -> Unix.kill reader Sys.sigusr1) readers

let rec waiting_readers trc readers =
  match Persistent.Hashset.cardinal readers with
  | 0 -> ()
  | _len when Ipc.is_empty trc ->
    waiting_readers trc readers
  | _len ->
    let pid = Ipc.dequeue trc in
    let pid = Int64.to_int pid in
    if Persistent.Hashset.mem readers pid
    then Persistent.Hashset.remove readers pid ;
    waiting_readers trc readers

let truncate
  : type v c. Ipc.t -> (v, c) fd -> readers:int Persistent.Hashset.t -> Persistent.memory -> len:int64 -> Persistent.memory
  = fun ipc -> function
  | Truncate _ -> fun ~readers:_ _memory ~len:_ -> failwith "Illegal truncate"
  | Truncate_and_file_descr (trc, fd) -> fun ~readers memory ~len ->
    let old = Unix.LargeFile.fstat fd in
    let len = Int64.(div (add len (of_int page_size)) (of_int page_size)) in
    let len = Int64.(mul len (of_int page_size)) in
    try
      let f _ipc =
        signal_readers readers ; waiting_readers trc readers ; msync memory ;
        Unix.LargeFile.ftruncate fd len ;
        let memory = Mmap.V1.map_file fd
          ~pos:0L Bigarray.char Bigarray.c_layout true [| Int64.to_int len |] in
        Bigarray.array1_of_genarray memory in
      Ipc.with_lock ~f ipc
    with Unix.Unix_error (err, f, arg) as exn ->
      Log.err (fun m -> m "%s(%s) : %s (ftruncate 'fd:%Ld ~len:%Ld)"
        f arg (Unix.error_message err) old.Unix.LargeFile.st_size len) ;
      raise exn

let rec remap trc path mmu _sigusr1 =
  let f _ipc =
    let fd = Unix.openfile path Unix.[ O_RDWR ] 0o644 in
    let len = (Unix.fstat fd).st_size in
    let memory = Mmap.V1.map_file fd ~pos:0L Bigarray.char Bigarray.c_layout true [| len |] in
    let memory = Bigarray.array1_of_genarray memory in
    Unix.close fd ; memory in
  Ipc.enqueue trc (Int64.of_int (Unix.getpid ())) ;
  let memory = Ipc.with_lock ~f (Persistent.ipc mmu) in
  Persistent.unsafe_set_memory mmu memory ;
  Log.info (fun m -> m "Reader updated its virtual memory.") ;
  Sys.set_signal Sys.sigusr1 (Signal_handle (remap trc path mmu))

let rec run
  : type a p q. p state -> (p, q, a) t -> q state * a
  = fun s m -> match m, s with
  | Return x, _ -> s, x
  | Bind (m, f), _ -> let s, x = run s m in run s (f x)
  | Find key, Opened (mmu, capabilities, fd) ->
    Opened (mmu, capabilities, fd),
    Persistent.(run mmu (Persistent.find mmu key))
  | Insert (key, value), Opened (mmu, capabilities, fd) ->
    Opened (mmu, capabilities, fd),
    Persistent.(run mmu (insert mmu key value))
  | Open (Reader uid, path), Closed ->
    let ipc = Ipc.connect (Fmt.str "%s.socket" path) in
    let trc = Ipc.connect (Fmt.str "%s-truncate.socket" path) in
    let f _ipc =
      let fd = Unix.openfile path Unix.[ O_RDWR ] 0o644 in
      let len = ((Unix.fstat fd).st_size + page_size) / page_size in
      let len = len * page_size in
      let memory = Mmap.V1.map_file fd
        ~pos:0L Bigarray.char Bigarray.c_layout true [| len |] in
      let memory = Bigarray.array1_of_genarray memory in
      Unix.close fd ; memory in
    let memory = Ipc.with_lock ~f ipc in
    let mmu = Persistent.ro ~truncate:(truncate ipc (Truncate trc)) ipc memory in
    Sys.set_signal Sys.sigusr1 (Signal_handle (remap trc path mmu)) ;
    (* TODO(dinosaure): keep [trc] to properly close it! *)
    Ipc.enqueue ipc uid ; Opened (mmu, Reader uid, Truncate trc), ()
  | Close, Opened (mmu, Reader uid, Truncate trc) ->
    let ipc = Persistent.ipc mmu in
    Ipc.enqueue ipc uid ;
    Ipc.close ipc ; Ipc.close trc ; Closed, ()
  | Open (Writer, path), Closed ->
    let fd = Unix.openfile path Unix.[ O_RDWR ] 0o644 in
    let len = ((Unix.fstat fd).st_size + page_size) / page_size in
    let len = len * page_size in
    let memory = Mmap.V1.map_file fd
      ~pos:0L Bigarray.char Bigarray.c_layout true [| len |] in
    let memory = Bigarray.array1_of_genarray memory in
    let ipc = Ipc.connect (Fmt.str "%s.socket" path) in
    let trc = Ipc.connect (Fmt.str "%s-truncate.socket" path) in
    let mmu = Persistent.rdwr
      ~truncate:(truncate ipc (Truncate_and_file_descr (trc, fd))) ipc memory in
    (* TODO(dinosaure): keep [trc] to properly close it! *)
    Opened (mmu, Writer, Truncate_and_file_descr (trc, fd)), ()
  | Create (path, len), Closed ->
    let fd = Unix.openfile path Unix.[ O_CREAT; O_RDWR ] 0o644 in
    let _  = Unix.lseek fd len Unix.SEEK_SET in
    let len = (len + page_size) / page_size in
    let len = len * page_size in
    let memory = Mmap.V1.map_file fd
      ~pos:0L Bigarray.char Bigarray.c_layout true [| len |] in
    let memory = Bigarray.array1_of_genarray memory in
    ( match Ipc.create (Fmt.str "%s.socket" path),
            Ipc.create (Fmt.str "%s-truncate.socket" path) with
    | Ok (), Ok () ->
      let ipc = Ipc.connect (Fmt.str "%s.socket" path) in
      let trc = Ipc.connect (Fmt.str "%s-truncate.socket" path) in
      let _mmu = Persistent.rdwr
        ~truncate:(truncate ipc (Truncate trc)) ipc memory in
      let _mmu = Persistent.run _mmu
        (Persistent.make ~truncate:(truncate ipc (Truncate trc)) ipc memory) in
      Ipc.close ipc ; Ipc.close trc ; Unix.close fd ; Closed, Ok ()
    | Error err, _ | _, Error err -> Closed, Error err )
  | Close, Opened (mmu, Writer, (Truncate_and_file_descr (trc, fd))) ->
    let ipc = Persistent.ipc mmu in
    Ipc.close ipc ; Ipc.close trc ;
    Unix.close fd ; Closed, ()   
