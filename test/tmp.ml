(* (c) Daniel Bünzli
 * without automatic deletion *)

open Rresult

let rand_gen = lazy (Random.State.make_self_init ())
(* XXX(dinosaure): local random generator. *)

let rand_path dir pat =
  let rand = Random.State.bits (Lazy.force rand_gen) land 0xFFFFFF in
  Fpath.(dir / Fmt.str pat (Fmt.str "%06x" rand))

let default_dir_init =
  let from_env var ~absent =
    match try Some (Sys.getenv var) with Not_found -> None with
    | None -> absent
    | Some v -> (
        match Fpath.of_string v with
        | Error (`Msg err) -> failwith err
        | Ok v -> v)
  in
  if Sys.os_type = "Win32" then from_env "TEMP" ~absent:Fpath.(v "./")
  else from_env "TMPDIR" ~absent:Fpath.(v "/tmp")

let default_dir = ref default_dir_init
let set_default_dir p = default_dir := p
let default_dir () = !default_dir

let create_tmp_path mode dir pat =
  let err () =
    R.error_msgf "create temporary file %s in %a: too many failing attemps"
      (Fmt.str pat "XXXXXX") Fpath.pp dir
  in
  let rec go count =
    if count < 0 then err ()
    else
      let file = rand_path dir pat in
      let sfile = Fpath.to_string file in
      let open_flags = Unix.[ O_WRONLY; O_CREAT; O_EXCL ] in
      try Ok (file, Unix.openfile sfile open_flags mode) with
      | Unix.Unix_error (Unix.EEXIST, _, _) -> go (count - 1)
      | Unix.Unix_error (Unix.EINTR, _, _) -> go count
      | Unix.Unix_error (e, _, _) ->
          R.error_msgf "create temporary file %a: %s" Fpath.pp file
            (Unix.error_message e)
  in
  go 1000

type pattern = (string -> string, Format.formatter, unit, string) format4

let tmp ?(mode = 0o644) ?dir pat =
  let dir = match dir with None -> default_dir () | Some d -> d in
  create_tmp_path mode dir pat >>= fun (file, fd) ->
  let rec close fd =
    try Unix.close fd with
    | Unix.Unix_error (Unix.EINTR, _, _) -> close fd
    | Unix.Unix_error _ -> ()
  in
  close fd;
  Ok file
