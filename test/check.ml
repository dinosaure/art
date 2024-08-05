module Impl = struct
  type 'a t = 'a

  let bind x f = f x
  let return x = x

  let atomic_get (t : 'a rd Addr.t) v =
    match vtable.(t :> int), v with
    | (Int8, cell), Int8 -> TracedAtomic.get cell
    | (LEInt16, cell), Int8 -> TracedAtomic.get cell land 0xf
    | (LEInt, cell), Int8 -> TracedAtomic.get cell land 0xf
    | (LEInt31, cell), Int8 -> TracedAtomic.get cell land 0xf
    | (LEInt64, cell), Int8 -> Int64.(to_int (logand (TracedAtomic.get cell) 0xfL))
end
