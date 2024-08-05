open Rowex

let src = Logs.Src.create "rowex.mem"

module Log = (val Logs.src_log src : Logs.LOG)

external bytes_get_uint16 : bytes -> int -> int = "%caml_bytes_get16"
external bytes_set_uint16 : bytes -> int -> int -> unit = "%caml_bytes_set16"
external bytes_get_uint32 : bytes -> int -> int32 = "%caml_bytes_get32"
external bytes_set_uint32 : bytes -> int -> int32 -> unit = "%caml_bytes_set32"
external bytes_get_uint64 : bytes -> int -> int64 = "%caml_bytes_get64"
external bytes_set_uint64 : bytes -> int -> int64 -> unit = "%caml_bytes_set64"
external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"

let bytes_get_leuint16 =
  if Sys.big_endian then fun buf idx -> swap16 (bytes_get_uint16 buf idx)
  else bytes_get_uint16

let bytes_get_leuint32 =
  if Sys.big_endian then fun buf idx -> swap32 (bytes_get_uint32 buf idx)
  else bytes_get_uint32

let bytes_get_leuint64 =
  if Sys.big_endian then fun buf idx -> swap64 (bytes_get_uint64 buf idx)
  else bytes_get_uint64

let bytes_set_leuint16 =
  if Sys.big_endian then fun buf idx v -> bytes_set_uint16 buf idx (swap16 v)
  else bytes_set_uint16

let bytes_set_leuint32 =
  if Sys.big_endian then fun buf idx v -> bytes_set_uint32 buf idx (swap32 v)
  else bytes_set_uint32

let bytes_set_leuint64 =
  if Sys.big_endian then fun buf idx v -> bytes_set_uint64 buf idx (swap64 v)
  else bytes_set_uint64

(** This module does not want to provide an optimised version of ROWEX nor
    a parallel version (compatible with OCaml 5). It provides a /dumb/
    implementation which can be used by fuzzers to test the ROWEX implementation
    regardless ACID properties.

    Don't use it! *)
module Make (Memory : sig
  val memory : bytes
end) =
struct
  type 'a t = 'a

  open Memory

  let bind x f = f x
  let return x = x

  let get : type c v. 'a Addr.t -> (c, v) value -> v t =
   fun addr t ->
    let addr = Addr.unsafe_to_int addr in
    (* TODO(dinosaure): [(addr :> int)] does not work, why? *)
    match t with
    | Int8 -> Bytes.get memory addr |> Char.code
    | LEInt when Sys.word_size = 32 ->
        bytes_get_leuint32 memory addr |> Int32.to_int
    | LEInt when Sys.word_size = 64 ->
        bytes_get_leuint64 memory addr |> Int64.to_int
    | LEInt16 -> bytes_get_leuint16 memory addr
    | LEInt31 -> bytes_get_leuint32 memory addr |> Int32.to_int
    | LEInt64 -> bytes_get_leuint64 memory addr
    | LEInt128 -> Bytes.sub_string memory addr (addr + 16)
    | Addr_rd when Sys.word_size = 32 ->
        bytes_get_leuint32 memory addr |> Int32.to_int |> Addr.of_int_to_rdonly
    | Addr_rdwr when Sys.word_size = 32 ->
        bytes_get_leuint32 memory addr |> Int32.to_int |> Addr.of_int_to_rdwr
    | Addr_rd when Sys.word_size = 64 ->
        bytes_get_leuint64 memory addr |> Int64.to_int |> Addr.of_int_to_rdonly
    | Addr_rdwr when Sys.word_size = 64 ->
        bytes_get_leuint64 memory addr |> Int64.to_int |> Addr.of_int_to_rdwr
    | C_string ->
        let buf = Buffer.create 0x10 in
        let idx = ref 0 in
        while Bytes.get memory (addr + !idx) <> '\000' do
          Buffer.add_char buf (Bytes.get memory (addr + !idx));
          incr idx
        done;
        Log.debug (fun m -> m "%016x loaded (%d byte(s)):" addr !idx);
        Log.debug (fun m ->
            m "@[<hov>%a@]" (Hxd_string.pp Hxd.default) (Buffer.contents buf));
        Buffer.contents buf
    | LEInt | Addr_rd | Addr_rdwr -> assert false

  let atomic_get : type v. 'a rd Addr.t -> (atomic, v) value -> v t =
   fun addr k -> get addr k

  let atomic_set : type v. 'a wr Addr.t -> (atomic, v) value -> v -> unit t =
   fun addr t v ->
    let addr = Addr.unsafe_to_int addr in
    match t with
    | Int8 -> Bytes.set memory addr (Char.chr v)
    | LEInt when Sys.word_size = 32 ->
        bytes_set_leuint32 memory addr (Int32.of_int v)
    | LEInt when Sys.word_size = 64 ->
        bytes_set_leuint64 memory addr (Int64.of_int v)
    | LEInt16 -> bytes_set_leuint16 memory addr v
    | LEInt31 -> bytes_set_leuint32 memory addr (Int32.of_int v)
    | LEInt64 -> bytes_set_leuint64 memory addr v
    | LEInt128 -> Bytes.blit memory addr (Bytes.of_string v) 0 16
    | Addr_rd when Sys.word_size = 32 ->
        bytes_set_leuint32 memory addr (Int32.of_int (Addr.unsafe_to_int v))
    | Addr_rdwr when Sys.word_size = 32 ->
        bytes_set_leuint32 memory addr (Int32.of_int (Addr.unsafe_to_int v))
    | Addr_rd when Sys.word_size = 64 ->
        bytes_set_leuint64 memory addr (Int64.of_int (Addr.unsafe_to_int v))
    | Addr_rdwr when Sys.word_size = 64 ->
        bytes_set_leuint64 memory addr (Int64.of_int (Addr.unsafe_to_int v))
    | LEInt | Addr_rd | Addr_rdwr -> assert false

  let now () = int_of_float (Unix.gettimeofday ())
  let free = Hashtbl.create 0x10
  let keep = Hashtbl.create 0x10
  let brk = ref 0

  let delete addr len =
    try
      let vs = Hashtbl.find free len in
      Hashtbl.add free len (Addr.unsafe_to_int addr :: vs)
    with Not_found -> Hashtbl.add free len [ Addr.unsafe_to_int addr ]

  let collect () =
    let commit = now () in
    Hashtbl.filter_map_inplace
      (fun time (addr, len) ->
        if time < commit then (
          delete (Addr.of_int_to_rdwr addr) len;
          None)
        else Some (addr, len))
      keep

  let lint ~kind addr len payloads =
    Bytes.blit_string (String.concat "" payloads) 0 memory addr len;
    if kind = `Node then
      bytes_set_leuint64 memory (addr + _header_owner) (Int64.of_int (now ()))

  let allocate ~kind ?len payloads =
    let len =
      match len with
      | Some len -> len
      | None -> List.fold_left (fun a s -> a + String.length s) 0 payloads
    in
    let rec alloc tries =
      if tries <= 0 then (
        if !brk + len > Bytes.length memory then raise Out_of_memory
        else
          let addr = !brk in
          lint ~kind addr len payloads;
          brk := !brk + len;
          Addr.of_int_to_rdwr addr)
      else
        match Hashtbl.find_opt free len with
        | None | Some [] ->
            collect ();
            alloc (pred tries)
        | Some (cell :: rest) ->
            Hashtbl.replace free len rest;
            lint ~kind cell len payloads;
            Addr.of_int_to_rdwr cell
    in
    alloc 1

  let collect : _ Addr.t -> len:int -> uid:int -> unit =
   fun addr ~len ~uid:time ->
    Hashtbl.add keep time (Addr.unsafe_to_int addr, len)

  let fetch_add : rdwr Addr.t -> (atomic, int) value -> int -> int t =
   fun addr t v ->
    let v' = get addr t in
    atomic_set addr t (v + v');
    v'

  let fetch_or : rdwr Addr.t -> (atomic, int) value -> int -> int t =
   fun addr t v ->
    let v' = get addr t in
    atomic_set addr t (v lor v');
    v'

  let fetch_sub : rdwr Addr.t -> (atomic, int) value -> int -> int t =
   fun addr t v ->
    let v' = get addr t in
    atomic_set addr t (v - v');
    v'

  let compare_exchange :
      type v.
      ?weak:bool ->
      rdwr Addr.t ->
      (atomic, v) value ->
      v Atomic.t ->
      v ->
      bool t =
   fun ?weak:_ addr t seen v ->
    let v' = get addr t in
    if v' = Atomic.get seen then (
      atomic_set addr t v;
      true)
    else false

  let pause_intrinsic = ()
  let persist _addr ~len:_ = ()
end
