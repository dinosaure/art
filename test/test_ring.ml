let pp_process_status ppf = function
  | Unix.WEXITED n -> Format.fprintf ppf "(WEXITED %d)" n
  | Unix.WSIGNALED n -> Format.fprintf ppf "(WSIGNALED %d)" n
  | Unix.WSTOPPED n -> Format.fprintf ppf "(WSTOPPED %d)" n

let res = ref true

let exit_success = 0
let exit_failure = 1

let properly_exited = function Unix.WEXITED 0 -> true | _ -> false

let run order len =
  let pid =
    Unix.create_process_env "./rb.exe"
      [| "./rb.exe"; string_of_int order; string_of_int len |]
      [||] Unix.stdin Unix.stdout Unix.stderr in
  let _, status = Unix.waitpid [] pid in
  res := !res && properly_exited status ;
  Format.printf ">>> ./rb.exe %d %d: %a.\n%!" order len pp_process_status status

let () =
  run 15 10 ;
  run 15 100 ;
  run 15 1000 ;
  if !res then exit exit_success else exit exit_failure
