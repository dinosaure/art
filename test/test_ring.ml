let pp_process_status ppf = function
  | Unix.WEXITED n -> Format.fprintf ppf "(WEXITED %d)" n
  | Unix.WSIGNALED n -> Format.fprintf ppf "(WSIGNALED %d)" n
  | Unix.WSTOPPED n -> Format.fprintf ppf "(WSTOPPED %d)" n

let res = ref true

let exit_success = 0
let exit_failure = 1

let properly_exited = function Unix.WEXITED 0 -> true | _ -> false

let count = ref 0

let show filename =
  let ic = open_in filename in
  let rec go ic = match input_line ic with
    | line -> Format.printf "%s\n%!" line ; go ic
    | exception End_of_file -> close_in ic in
  go ic

let run order len =
  let filename = Format.asprintf "%02d-ring.log" !count in
  incr count ;
  let log = Unix.openfile filename Unix.[ O_RDWR; O_CREAT; O_TRUNC ] 0o644 in
  let pid =
    Unix.create_process_env "./rb.exe"
      [| "./rb.exe"; "--tmp=./tmp"; string_of_int order; string_of_int len |]
      [||] Unix.stdin Unix.stdout log in
  let _, status = Unix.waitpid [] pid in
  Unix.close log ; res := !res && properly_exited status ;
  Format.printf ">>> ./rb.exe --tmp=./tmp %d %d: %a.\n%!" order len pp_process_status status ;
  if not (properly_exited status)
  then show filename

let () =
  run 15 10 ;
  run 15 100 ;
  run 15 1000 ;
  if !res then exit exit_success else exit exit_failure
