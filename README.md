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

The function `prefix_iter` is also available if you want to get a subset of your
tree:
```ocaml
# let t = Art.make () ;;
# Art.insert t (Art.key
# Art.insert t (Art.key "Dalton Joe") 0 ;;
# Art.insert t (Art.key "Dalton Jack") 1 ;;
# Art.insert t (Art.key "Dalton William") 2 ;;
# Art.insert t (Art.key "Dalton Averell") 3 ;;
# Art.insert t (Art.key "Rantanplan") 4 ;;
# let dalton = Art.prefix_iter ~prefix:(Art.key "Dalton")
  (fun k _ a -> (k :> string) :: a) [] t ;;
- : string list = [ "Dalton Joe"
                  ; "Dalton Jack"
		  ; "Dalton William"
		  ; "Dalton Averell" ]
```

[ART]: https://db.in.tum.de/~leis/papers/ART.pdf
[find-bechamel]: https://dinosaure.github.io/art/bench/find.html
[insert-bechamel]: https://dinosaure.github.io/art/bench/insert.html
