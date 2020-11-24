let _m_amd64 = "_M_AMD64"
let _m_x64 = "_M_X64"
let _m_ix86_fp = "_MIX86_FP"
let __sse2__ = "__SSE2__"
let __ssse3__ = "__SSSE3__"
let __aarch64__ = "__aarch64__"
let __arm_neon = "__ARM_NEON"
let __i386__ = "__i386__"
let _msc_ver = "_MSC_VER"

open Configurator.V1.C_define.Value

let sse2_support lst =
  match List.assoc_opt _m_amd64 lst,
        List.assoc_opt _m_x64 lst,
        List.assoc_opt _m_ix86_fp lst,
        List.assoc_opt __sse2__ lst with
  | Some (Switch true), _, _, _ -> true
  | _, Some (Switch true), _, _ -> true
  | _, _, Some (Int 2), _       -> true
  | _, _, _, Some (Switch true) -> true
  | _ -> false

let ssse3_support lst =
  match List.assoc_opt __ssse3__ lst with
  | Some (Switch v) -> v
  | _ -> false

let neon_support lst =
  match List.assoc_opt __aarch64__ lst,
        List.assoc_opt __arm_neon lst with
  | Some (Switch true), _ -> true
  | _, Some (Switch true) -> true
  | _ -> false

let _ =
  let c = Configurator.V1.create "sse" in
  let defines = Configurator.V1.C_define.import
    c ~includes:[]
    [ (_m_amd64, Switch)
    ; (_m_x64, Switch)
    ; (__i386__, Switch)
    ; (_msc_ver, Switch)
    ; (__sse2__, Switch)
    ; (__ssse3__, Switch)
    ; (__aarch64__, Switch)
    ; (__arm_neon, Switch) ] in
  let defines = match List.assoc_opt __i386__ defines,
                      List.assoc_opt _msc_ver defines with
    | Some (Switch true), Some (Switch true) ->
      Configurator.V1.C_define.import c ~includes:[] [ (_m_ix86_fp, Int) ] @ defines
    | _ -> defines in
  let flags =
    match sse2_support defines,
          ssse3_support defines,
          neon_support defines with
    | true, true, false  -> [ "-DART_SSSE3"; "-mssse3"
                            ; "-DART_SSE2"; "-msse2" ]
    | true, false, false -> [ "-DART_SSE2"; "-msse2" ]
    | false, false, true -> [ "-DART_NEON"; ]
    | _ -> [] in
  Configurator.V1.Flags.write_sexp "sse.sexp" flags
