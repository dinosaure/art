let src = Logs.Src.create "ipc"

module Log = (val Logs.src_log src : Logs.LOG)

external swap64 : int64 -> int64 = "%bswap_int64"
external get_uint64 : string -> int -> int64 = "%caml_string_get64"
external set_uint64 : bytes -> int -> int64 -> unit = "%caml_bytes_set64"

let get_leuint64 str off =
  if Sys.big_endian then swap64 (get_uint64 str off) else get_uint64 str off

let set_leuint64 buf off v =
  if Sys.big_endian then set_uint64 buf off (swap64 v) else set_uint64 buf off v

let rec fully_read_leint64 fd =
  let tmp = Bytes.create 8 in
  go fd tmp 0 8

and go fd tmp off len =
  if len = 0 then get_leuint64 (Bytes.unsafe_to_string tmp) 0
  else
    let len' = Unix.read fd tmp off len in
    go fd tmp (off + len') (len - len')

let rec fully_write_leint64 fd v =
  let tmp = Bytes.create 8 in
  set_leuint64 tmp 0 v;
  go fd tmp 0 8

and go fd tmp off len =
  if len > 0 then
    let len' = Unix.write fd tmp off len in
    go fd tmp (off + len') (len - len')

open Rresult

type t = Unix.file_descr

let is_empty t =
  match Unix.select [ t ] [] [] 0. with [ _ ], _, _ -> false | _ -> true

let dequeue t =
  Unix.lockf t Unix.F_LOCK 0;
  let v = fully_read_leint64 t in
  Log.debug (fun m -> m "dequeue %Ld" v);
  Unix.lockf t Unix.F_ULOCK 0;
  v

let enqueue t v =
  Unix.lockf t Unix.F_LOCK 0;
  Log.debug (fun m -> m "enqueue %Ld" v);
  fully_write_leint64 t v;
  Unix.lockf t Unix.F_ULOCK 0

let close t = Unix.close t
let connect path = Unix.openfile path Unix.[ O_RDWR ] 0o600

let create path =
  try
    Unix.mkfifo path 0o600;
    R.ok ()
  with
  | Unix.Unix_error (err, f, v) ->
      R.error_msgf "%s(%s): %s" f v (Unix.error_message err)
  | exn -> raise exn

let with_lock ~f t =
  Unix.lockf t Unix.F_LOCK 0;
  let v = f t in
  Unix.lockf t Unix.F_ULOCK 0;
  v
