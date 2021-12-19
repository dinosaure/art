## Adaptive Radix Tree (ART) in OCaml

This is an implementation in OCaml of [ART][ART]. Adaptive Radix Tree is like a
simple `Hashtbl` with order:

```ocaml
# let tree = Art.make () ;;
# Art.insert tree (Art.key "foo") 42 ;;
# Art.insert tree (Art.key "bar") 21 ;;
# Art.find tree (Art.key "foo")
- : int = 42
```

Operation like `minimum` or `maximum` are available (which don't exist for a
simple `Hashtbl.t`):

```ocaml
# let tree = Art.make () ;;
# Art.insert tree (Art.key "0") 0 ;;
# Art.insert tree (Art.key "1") 1 ;;
# Art.insert tree (Art.key "2") 2 ;;
# Art.minimum tree
- : int = 0
# Art.maximum tree
- : int = 2
```

If you want the order and the speed of `Hashtbl.t`, Art is your library:
- Benchmark on [`find`][find-bechamel]
- Benchmark on [`insert`][insert-bechamel]

## Read Optimised Write Exclusion (ROWEX) in OCaml

ROWEX is a second implementation of ART with atomic operations. It's a _functor_
which expects an implementation of atomic operations such as `load` or `store`.

### Parallelism, atomic operation & OCaml

The current version of OCaml has a global lock for the GC. By this way, it's not
possible for us to execute ROWEX operations (`find`/`insert`) with true
parallelism if we use the same OCaml runtime. Even if you use LWT or ASYNC, you
execute jobs concurrently.

However, ROWEX wants to provide an implementation where `find`/`insert` can be
executed in parallel without any problems (race condition or ABA problem). So
ROWEX provides an implementation, `persistent`, which implements atomic
operations on a memory area. Then, we are able, as [`parmap`][parmap], to
simulate true parallelism as long as each operations are executed into their own
[`fork()`][fork].

The goal of this library is provide:
- the most easy way to switch the implementation to
  [ocaml-multicore][ocaml-multicore]
- a baby step to be able to manipulate a file by several processes
  (consumers/`find`, producers/`insert`) in parallel

ROWEX follows two main papers:
- The initial implementation of [ROWEX][ROWEX]
- A derivation of it to be **persistent**: [PART][PART]

### Tools

The distribution comes with some tools to manipulate an _index_:
```sh
$ opam pin add -y https://github.com/dinosaure/art
$ opam install rowex
$ part.make index.idx
$ ls -lh
-rw-r--r-- 1 user user 8,0M ----- -- --:-- index.idx
prw------- 1 user user    0 ----- -- --:-- index.idx.socket
prw------- 1 user user    0 ----- -- --:-- index.idx-truncate.socket
$ part.insert index.idx foo 1
$ part.find index.idx foo
1
```

On the OCaml side, a `Part` module exists which implements these functions:
```ocaml
type 'a t constraint 'a = [< `Rd | `Wr ]

val create : ?len:int -> string -> unit
val insert : [> `Rd | `Wr ] t -> string -> int -> unit
val lookup : [> `Rd ] t -> string -> int
```

`part` is Unix dependent (and it need an **Unix named pipe**). It ensures with
explained internal mechanisms to use multiple readers and one writer:
- The _writer_ can take the exclusive ownership on the index file and its named
  pipe
- _readers_ don't need to take the ownership but they must send a signal into
  the named pipe (to the _writer_) that they start to introspect the index

For _readers_, some functions exist to signal their existence to the _write_:
```ocaml
val append_reader : Ipc.t -> unit
val delete_reader : Ipc.t -> unit

val ipc : _ t -> Ipc.t
```
 
### Status: experimental

This part of the distribution is **experimental** - even if the distribution
comes with several tests to ensure that the implementation works, ROWEX is
fragile! It still need a synchronization mechanism `fsync()` which is added
pervasively in some parts of the code according to outcomes of errors.

[ART]: https://db.in.tum.de/~leis/papers/ART.pdf
[ROWEX]: https://db.in.tum.de/~leis/papers/artsync.pdf
[PART]: https://arxiv.org/pdf/1909.13670.pdf
[find-bechamel]: https://dinosaure.github.io/art/bench/find.html
[insert-bechamel]: https://dinosaure.github.io/art/bench/insert.html
[parmap]: https://github.com/rdicosmo/parmap
[fork]: https://man7.org/linux/man-pages/man2/fork.2.html
[ocaml-multicore]: https://github.com/ocaml-multicore/ocaml-multicore
