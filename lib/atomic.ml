type 'a t = { mutable v : 'a }

let make v = { v }
let get r = r.v
let set r v = r.v <- v

let exchange r v =
  let cur = r.v in
  r.v <- v ; cur

let compare_and_set r seen v =
  let cur = r.v in
  if cur == seen then ( r.v <- v ; true )
  else false

let fetch_and_add r n = 
  let cur = r.v in
  r.v <- (cur + n) ; cur

let incr r = ignore (fetch_and_add r 1)
let decr r = ignore (fetch_and_add r (-1))
