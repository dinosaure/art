let test01 =
  Alcotest.test_case "test01" `Quick @@ fun () ->
  let tree = Art.make () in
  Art.insert tree (Art.key "") 0 ;
  Art.insert tree (Art.key "") 1
;;

let test02 =
  Alcotest.test_case "test02" `Quick @@ fun () ->
  let tree = Art.make () in
  Art.insert tree (Art.key "") 0 ;
  Art.insert tree (Art.key "\042") 1 ;
  Art.insert tree (Art.key "") 2 ;
  let res = Art.find_opt tree (Art.key "toto") in
  Alcotest.(check (option int)) "res" res None
;;

let test03 =
  Alcotest.test_case "test03" `Quick @@ fun () ->
  let tree = Art.make () in
  Art.insert tree (Art.key "") 0 ;
  Art.insert tree (Art.key "\042") 1 ;
  let res0 = Art.find_opt tree (Art.key "") in
  let res1 = Art.find_opt tree (Art.key "toto") in
  Alcotest.(check (option int)) "res0" res0 (Some 0) ;
  Alcotest.(check (option int)) "res1" res1 None
;;

let test04 =
  Alcotest.test_case "test04" `Quick @@ fun () ->
  let tree = Art.make () in
  Art.insert tree (Art.key "") 1 ;
  Art.insert tree (Art.key "[\128") 2 ;
  Art.insert tree (Art.key "") 3 ;
  Art.insert tree (Art.key "\025\025\b7\025\128") 4 ;
  let res = Art.find_opt tree (Art.key "\003") in
  Alcotest.(check (option int)) "res" res None
;;

let test05 =
  Alcotest.test_case "test05" `Quick @@ fun () ->
  let tree = Art.make () in
  Art.insert tree (Art.key "toto") 1 ;
  Art.insert tree (Art.key "") 2 ;
  Art.insert tree (Art.key "titi") 3 ;
  Art.insert tree (Art.key "") 4 ;
  let res0 = Art.find_opt tree (Art.key "toto") in
  let res1 = Art.find_opt tree (Art.key "") in
  let res2 = Art.find_opt tree (Art.key "titi") in
  Alcotest.(check (option int)) "res0" res0 (Some 1) ;
  Alcotest.(check (option int)) "res1" res1 (Some 4) ;
  Alcotest.(check (option int)) "res2" res2 (Some 3)
;;

let test06 =
  Alcotest.test_case "test06" `Quick @@ fun () ->
  let tree = Art.make () in
  Art.insert tree (Art.key "toto") 1 ;
  Art.insert tree (Art.key "tutu") 2 ;
  Art.insert tree (Art.key "titi") 3 ;
  Art.insert tree (Art.key "") 4 ;
  let res0 = Art.find_opt tree (Art.key "toto") in
  let res1 = Art.find_opt tree (Art.key "tutu") in
  let res2 = Art.find_opt tree (Art.key "titi") in
  let res3 = Art.find_opt tree (Art.key "") in
  Alcotest.(check (option int)) "res0" res0 (Some 1) ;
  Alcotest.(check (option int)) "res1" res1 (Some 2) ;
  Alcotest.(check (option int)) "res2" res2 (Some 3) ;
  Alcotest.(check (option int)) "res3" res3 (Some 4)
;;

let test07 =
  Alcotest.test_case "test07" `Quick @@ fun () ->
  let tree = Art.make () in
  let res0 = Art.find_opt tree (Art.key "") in
  Art.insert tree (Art.key "\241\241") 1 ;
  Art.insert tree (Art.key "\241\241\003\232") 2 ;
  Alcotest.(check (option int)) "res0" res0 None
;;

let test08 =
  Alcotest.test_case "test08" `Quick @@ fun () ->
  let tree = Art.make () in
  let res0 = Art.find_opt tree (Art.key "foo") in
  Art.insert tree (Art.key "\251\250\250\250\250") 1 ;
  Art.insert tree (Art.key "\251\250\250") 2 ;
  let res1 = Art.find_opt tree (Art.key "\251\250\250\250\250") in
  let res2 = Art.find_opt tree (Art.key "\251\250\250") in
  Alcotest.(check (option int)) "res0" res0 None ;
  Alcotest.(check (option int)) "res1" res1 (Some 1) ;
  Alcotest.(check (option int)) "res2" res2 (Some 2)
;;

let test09 =
  Alcotest.test_case "test09" `Quick @@ fun () ->
  let tree = Art.make () in
  let res0 = Art.find_opt tree (Art.key "foo") in
  Art.insert tree (Art.key "\185\185\001\001") 1 ;
  Art.insert tree (Art.key "\185\185") 2 ;
  Art.insert tree (Art.key "\185\185") 3 ;
  let res1 = Art.find_opt tree (Art.key "\185\185") in
  Alcotest.(check (option int)) "res0" res0 None ;
  Alcotest.(check (option int)) "res1" res1 (Some 3)
;;

let test10 =
  Alcotest.test_case "test10" `Quick @@ fun () ->
  let tree = Art.make () in
  Art.insert tree (Art.key "\134") 1 ;
  Art.insert tree (Art.key "\021\n\135\131\2105\161=\142\028\152\006\031\188\191\201\134\194Fu\253\132\1279v_\200&\238") 2 ;
  Art.insert tree (Art.key "\182\206\219:n4\156\173Mu\012\223;\215\197") 3 ;
  Art.insert tree (Art.key "bG>\229") 4 ;
  Art.insert tree (Art.key "[\183\191\201\169M\210\183\015oK%\252\021\187\158e\138\202\193\129\145-") 5 ;
  Art.insert tree (Art.key "\191\175\151\144\012\226\211Y\182\226 +\245\153\204\002\148\149\219^4Aq\189\201\003") 6 ;
  Art.insert tree (Art.key "\182\024\023\145\178\206\\wW0\138\157\223\184]\137g\031\029\"k\132") 7 ;
  Art.insert tree (Art.key "'g\253s\029?\243b]l\197\197^\168\244") 8 ;
  Art.insert tree (Art.key "\027^L") 9 ;
  Art.insert tree (Art.key "-^\213\151") 10 ;
  Art.insert tree (Art.key "\004") 11 ;
  Art.insert tree (Art.key "\\\217\023\023\023\200\131\226\250\246") 12 ;
  Art.insert tree (Art.key "\207\251y\235\214\164`6\017)h(  \179\215]5'\151\178\208") 13 ;
  Art.insert tree (Art.key "\n\209 \\\235z^\183") 14 ;
  Art.insert tree (Art.key "]\218") 15 ;
  Art.insert tree (Art.key "\127\252N\155\2162\145") 16 ;
  Art.insert tree (Art.key "|V\232Z\210&O\234\197\229w\127O\250\154h\236\224\254") 17 ;
  Art.insert tree (Art.key "A)\139\0289\017\020\194\192<\143.!") 18 ;
  Art.insert tree (Art.key "\153\207e\194\027\\\128\016U") 19 ;
  Art.insert tree (Art.key "\130:\228PMO;IA\253|=\169<w\001\197\028") 20 ;
  Art.insert tree (Art.key "\170\025>\023\031X\233\181gdx") 21 ;
  Art.insert tree (Art.key "\177\144w\215\250\024\247") 22 ;
  Art.insert tree (Art.key "\016\242\174\217\\\173o\134\182,}\240\193\179") 23 ;
  Art.insert tree (Art.key "\161\132?Uy\162v\228\023\015G&F>\177") 24 ;
  Art.insert tree (Art.key "\195:\018\180 \004\152\018\143\231z\212\255\024W\159\147|\006\2069Z\218\006\006\241\166u\167N\024\019'") 25 ;
  Art.insert tree (Art.key "\203\162\138\133\129_a(\178\028\177") 26 ;
  Art.insert tree (Art.key "\250\027\150\191\162\216;\024\023\245%\238\224") 27 ;
  Art.insert tree (Art.key "\\\020\224\t\253U\231\022-\t\249\151Z\014") 28 ;
  Art.insert tree (Art.key "\147\238\011y(\247\135}K\1952W\004~n") 29 ;
  Art.insert tree (Art.key "\130\018Iz|\165l\177_\161N\187#\205\159*)") 30 ;
  Art.insert tree (Art.key "\222\226\200\005fM") 31 ;
  Art.insert tree (Art.key "\224o;Zu\192\248[\144Q?\167\131\194\1608\014j\224\254") 32 ;
  Art.insert tree (Art.key "\219jsv\n\226") 33 ;
  Art.insert tree (Art.key "\003@3\150\229\tJ\134") 34 ;
  Art.insert tree (Art.key "\022$") 35 ;
  Art.insert tree (Art.key "\198\156\241\138ga9\239\237\212\151\189\1311\220\247") 36 ;
  Art.insert tree (Art.key "\210\156\146s\212a\187\244Up\022U\019\207\179\244") 37 ;
  Art.insert tree (Art.key "\145\015\001\210\n\142\215P\003A\137k\218\130>\b\017") 38 ;
  Art.insert tree (Art.key "\241\165\1348\226") 39 ;
  Art.insert tree (Art.key "\211\205\132h") 39 ;
  Art.insert tree (Art.key "\220\255\153\015N\167@KA\183n\141") 40 ;
  Art.insert tree (Art.key "\184O\129\147\031\225\168>\182\210\252\211\255!\234\157\134\153\198E\169w\217") 41 ;
  Art.insert tree (Art.key "\157z\229L\236]\2548\015w\174V\011\250\n\135~U^\139#\180") 42 ;
  Art.insert tree (Art.key "\233v\250\2500\218\184\195\177r\003p") 43 ;
  Art.insert tree (Art.key "Yu\177C>\165-\228") 44 ;
  Art.insert tree (Art.key "wA\191") 45 ;
  Art.insert tree (Art.key ",\031\253\195\031\\\135eb\014\189q\183\023\180\184b") 46 ;
  Art.insert tree (Art.key "\006\254\211}") 47 ;
  Art.insert tree (Art.key "\137;\155\134qD\225T\193\"\005\133\012H\019]N") 48 ;
  Art.insert tree (Art.key "\020*\017\225q\147\198\2532\253\171B\184u=7\174s\209\174r\011\224\030") 49 ;
  Art.insert tree (Art.key "\014R\220\202&") 50 ;
  Art.insert tree (Art.key "O\128)\231\192O\184f{\223\232") 51 ;
  Alcotest.(check int) "N48 -> N256" (Art.find tree (Art.key "O\128)\231\192O\184f{\223\232")) 51
;;

let test11 =
  Alcotest.test_case "test11" `Quick @@ fun () ->
  let tree = Art.make () in
  Art.insert tree (Art.key "foo") 1 ;
  Alcotest.(check int) "foo" (Art.find tree (Art.key "foo")) 1 ;
  Art.insert tree (Art.key "fo") 2 ;
  Alcotest.(check int) "foo" (Art.find tree (Art.key "foo")) 1 ;
  Alcotest.(check int) "fo" (Art.find tree (Art.key "fo")) 2 ;
  Art.insert tree (Art.key "foobar") 3 ;
  Alcotest.(check int) "foo" (Art.find tree (Art.key "foo")) 1 ;
  Alcotest.(check int) "fo" (Art.find tree (Art.key "fo")) 2 ;
  Alcotest.(check int) "foobar" (Art.find tree (Art.key "foobar")) 3
;;

let test12 =
  Alcotest.test_case "test12" `Quick @@ fun () ->
  let tree = Art.make () in
  Art.insert tree (Art.key "a0") 0 ;
  Art.insert tree (Art.key "a1") 1 ;
  Art.insert tree (Art.key "a2") 2 ;
  Art.insert tree (Art.key "a3") 3 ;
  Alcotest.(check int) "a0" (Art.find tree (Art.key "a0")) 0 ;
  Alcotest.(check int) "a1" (Art.find tree (Art.key "a1")) 1 ;
  Alcotest.(check int) "a2" (Art.find tree (Art.key "a2")) 2 ;
  Alcotest.(check int) "a3" (Art.find tree (Art.key "a3")) 3
;;

let test13 =
  Alcotest.test_case "test13" `Quick @@ fun () ->
  let k1 = Art.key (String.make 15 'a' ^ "foo") in
  let k2 = Art.key (String.make 15 'a' ^ "bar") in
  let k3 = Art.key (String.make 20 'a' ^ "foobar") in
  let k4 = Art.key (String.make 20 'a' ^ "barfoo") in
  let tree = Art.make () in
  Art.insert tree k1 1 ;
  Art.insert tree k2 2 ;
  Art.insert tree k3 3 ;
  Art.insert tree k4 4 ;
  Alcotest.(check int) (k1 :> string) (Art.find tree k1) 1 ;
  Alcotest.(check int) (k2 :> string) (Art.find tree k2) 2 ;
  Alcotest.(check int) (k3 :> string) (Art.find tree k3) 3 ;
  Alcotest.(check int) (k4 :> string) (Art.find tree k4) 4
;;

let test14 =
  Alcotest.test_case "test14" `Quick @@ fun () ->
  let k1 = Art.key (String.make 15 'a' ^ "foo") in
  let k2 = Art.key (String.make 15 'a' ^ "bar") in
  let k3 = Art.key (String.make 11 'a') in
  let k4 = Art.key (String.make 10 'a') in
  let k5 = Art.key (String.make 12 'a') in
  let tree = Art.make () in
  Art.insert tree k1 1 ;
  Art.insert tree k2 2 ;
  Art.insert tree k3 3 ;
  Alcotest.(check int) (k1 :> string) (Art.find tree k1) 1 ;
  Alcotest.(check int) (k2 :> string) (Art.find tree k2) 2 ;
  Alcotest.(check int) (k3 :> string) (Art.find tree k3) 3 ;
  (* XXX(dinosaure): see around out of prefix. *)
  Alcotest.check_raises (k4 :> string) Not_found (fun () -> ignore @@ Art.find tree k4) ;
  Alcotest.check_raises (k5 :> string) Not_found (fun () -> ignore @@ Art.find tree k5)
;;

let test15 =
  Alcotest.test_case "test15" `Quick @@ fun () ->
  let ks = Array.init 200 (function 0 -> Art.key "" | i -> Art.key (String.make 1 (Char.unsafe_chr i))) in
  let tree = Art.make () in
  Array.iteri (fun i k -> Art.insert tree k i) ks ;
  Alcotest.check_raises "not found" Not_found (fun () -> ignore @@ Art.find tree (Art.key "\255"))
;;

let key = Alcotest.testable (fun ppf (v : Art.key) -> Fmt.pf ppf "%S" (v :> string))
        (fun (a:Art.key) (b:Art.key) -> String.equal (a:>string) (b:>string))

let test16 =
  Alcotest.test_case "test16" `Quick @@ fun () ->
  let k1 = Art.key "\001" in
  let k2 = Art.key "\002" in
  let k3 = Art.key "\003" in
  let k4 = Art.key "\004" in
  let tree = Art.make () in
  Art.insert tree k4 4 ;
  Art.insert tree k3 3 ;
  Art.insert tree k2 2 ;
  Art.insert tree k1 1 ;
  Alcotest.(check (pair key int)) "minimum" (Art.minimum tree) (k1, 1)
;;

let test17 =
  Alcotest.test_case "test17" `Quick @@ fun () ->
  let ks = Array.init 16 (fun i -> Art.key (String.make 1 (Char.unsafe_chr (i + 1)))) in
  let tree = Art.make () in
  Array.iteri (fun i k -> Art.insert tree k i) ks ;
  Alcotest.(check (pair key int)) "minimum" (Art.minimum tree) (Art.key "\001", 0) ;
;;

let test18 =
  Alcotest.test_case "test18" `Quick @@ fun () ->
  let ks = Array.init 48 (fun i -> Art.key (String.make 1 (Char.unsafe_chr (i + 48)))) in
  let tree = Art.make () in
  Array.iteri (fun i k -> Art.insert tree k i) ks ;
  Alcotest.(check (pair key int)) "minimum" (Art.minimum tree) (Art.key "\048", 0) ;
;;

let test19 =
  Alcotest.test_case "test19" `Quick @@ fun () ->
  let ks = Array.init 256 @@ function
          | 0 -> Art.key ""
          | n -> Art.key (String.make 1 (Char.unsafe_chr n)) in
  let tree = Art.make () in
  Array.iteri (fun i k -> Art.insert tree k i) ks ;
  Alcotest.(check (pair key int)) "minimum" (Art.minimum tree) (Art.key "", 0) ;
;;

let test20 =
  Alcotest.test_case "test20" `Quick @@ fun () ->
  let tree = Art.make () in
  Alcotest.check_raises "minimum" (Invalid_argument "empty tree") @@ fun () -> ignore (Art.minimum tree) ;
;;

let test21 =
  Alcotest.test_case "test21" `Quick @@ fun () ->
  let tree = Art.make () in
  let foo = Art.key "foo" in
  Art.insert tree foo () ;
  Alcotest.(check unit) "find" (Art.find tree foo) () ;
  Art.remove tree foo ;
  Alcotest.check_raises "find" Not_found @@ fun () -> ignore (Art.find tree foo) ;
;;

let test22 =
  Alcotest.test_case "test22" `Quick @@ fun () ->
  let k0 = Art.key "foo\001" in
  let k1 = Art.key "foo\002" in
  let tree = Art.make () in
  Art.insert tree k0 () ;
  Art.insert tree k1 () ;
  Alcotest.(check unit) "find" (Art.find tree k0) () ;
  Alcotest.(check unit) "find" (Art.find tree k0) () ;
  Art.remove tree k0 ;
  Alcotest.check_raises "find" Not_found (fun () -> ignore (Art.find tree k0)) ;
  Alcotest.(check unit) "find" (Art.find tree k1) () ;
  Art.remove tree k1 ;
  Alcotest.check_raises "find" Not_found (fun () -> ignore (Art.find tree k1)) ;
;;

let test23 =
  Alcotest.test_case "test23" `Quick @@ fun () ->
  let tree = Art.make () in
  Alcotest.check_raises "remove" Not_found (fun () -> Art.remove tree (Art.key "a")) ;
  Art.insert tree (Art.key "a")  () ;
  Art.insert tree (Art.key "b")  () ;
  Alcotest.check_raises "remove" Not_found (fun () -> Art.remove tree (Art.key "c")) ;
  for i = 0x7f to 0xff do Art.insert tree (Art.key (String.make 1 (Char.unsafe_chr i))) () done ;
  Alcotest.check_raises "remove" Not_found (fun () -> Art.remove tree (Art.key "c")) ;
  Art.remove tree (Art.key "a") ;
  Alcotest.check_raises "find" Not_found (fun () -> ignore (Art.find tree (Art.key "a"))) ;
;;

let test24 =
  Alcotest.test_case "test24" `Quick @@ fun () ->
  let tree = Art.make () in
  Art.insert tree (Art.key "f") () ;
  Art.insert tree (Art.key "foo") () ;
  Art.insert tree (Art.key "foobar") () ;
  Alcotest.(check unit) "find" (Art.find tree (Art.key "f")) () ;
  Alcotest.(check unit) "find" (Art.find tree (Art.key "foo")) () ;
  Alcotest.(check unit) "find" (Art.find tree (Art.key "foobar")) () ;
  Art.remove tree (Art.key "foobar") ;
  Alcotest.(check unit) "find" (Art.find tree (Art.key "f")) () ;
  Alcotest.(check unit) "find" (Art.find tree (Art.key "foo")) () ;
  Alcotest.check_raises "find" Not_found (fun () -> ignore (Art.find tree (Art.key "foobar"))) ;
;;

let test25 =
  Alcotest.test_case "test25" `Quick @@ fun () ->
  let tree = Art.make () in
  Art.insert tree (Art.key "foo") 0 ;
  Art.insert tree (Art.key "foobar") 1 ;
  Art.insert tree (Art.key "foobar!") 2 ;
  Alcotest.(check int) "find" (Art.find tree (Art.key "foo")) 0 ;
  Alcotest.(check int) "find" (Art.find tree (Art.key "foobar")) 1 ;
  Alcotest.(check int) "find" (Art.find tree (Art.key "foobar!")) 2 ;
  Art.remove tree (Art.key "foo") ;
  Alcotest.(check int) "find" (Art.find tree (Art.key "foobar")) 1 ;
  Alcotest.(check int) "find" (Art.find tree (Art.key "foobar!")) 2 ;
  Alcotest.check_raises "find" Not_found (fun () -> ignore (Art.find tree (Art.key "foo"))) ;
;;

let test26 =
  Alcotest.test_case "test26" `Quick @@ fun () ->
  let tree = Art.make () in
  for i = 0 to 4 do Art.insert tree (Art.key (String.make 1 (Char.chr (i + 48)))) () done ;
  Alcotest.(check unit) "find" (Art.find tree (Art.key "0")) () ;
  Alcotest.(check unit) "find" (Art.find tree (Art.key "1")) () ;
  Alcotest.(check unit) "find" (Art.find tree (Art.key "2")) () ;
  Alcotest.(check unit) "find" (Art.find tree (Art.key "3")) () ;
  Alcotest.(check unit) "find" (Art.find tree (Art.key "4")) () ;
  Art.remove tree (Art.key "4") ;
  Art.remove tree (Art.key "3") ;
  Alcotest.(check unit) "find" (Art.find tree (Art.key "0")) () ;
  Alcotest.(check unit) "find" (Art.find tree (Art.key "1")) () ;
  Alcotest.(check unit) "find" (Art.find tree (Art.key "2")) () ;
  Alcotest.check_raises "find" Not_found (fun () -> ignore (Art.find tree (Art.key "4"))) ;
  Alcotest.check_raises "find" Not_found (fun () -> ignore (Art.find tree (Art.key "3"))) ;
;;

let test27 =
  Alcotest.test_case "test27" `Quick @@ fun () ->
  let tree = Art.make () in
  for i = 0 to 17 do Art.insert tree (Art.key (String.make 1 (Char.chr (i + 65)))) () done ;
  Alcotest.(check pass) "remove" (Art.remove tree (Art.key "A")) () ;
  Alcotest.(check pass) "remove" (Art.remove tree (Art.key "B")) () ;
  Alcotest.(check pass) "remove" (Art.remove tree (Art.key "C")) () ;
  Alcotest.(check pass) "remove" (Art.remove tree (Art.key "D")) () ;
  Alcotest.(check pass) "remove" (Art.remove tree (Art.key "E")) () ;
  Alcotest.(check pass) "remove" (Art.remove tree (Art.key "F")) () ;
;;

let test28 =
  Alcotest.test_case "test28" `Quick @@ fun () ->
  let tree = Art.make () in
  for i = 1 to 50 do Art.insert tree (Art.key (String.make 1 (Char.chr i))) () done ;
  for i = 1 to 38 do
    Alcotest.(check pass) "remove" (Art.remove tree (Art.key (String.make 1 (Char.chr i)))) ()
  done ;
;;

let test29 =
  Alcotest.test_case "test29" `Quick @@ fun () ->
  let tree = Art.make () in
  for i = 1 to 50 do Art.insert tree (Art.key (String.make 1 (Char.chr i))) i done ;
  let f (key:Art.key) value acc =
    Alcotest.(check int) "iter" (Char.code (key :> string).[0]) value ;
    succ acc in
  Alcotest.(check int) "iter" (Art.iter ~f 0 tree) 50 ;
;;

let () =
  Alcotest.run "art"
    [ "art", [ test01
             ; test02
             ; test03
             ; test04
             ; test05
             ; test06
             ; test07
             ; test08
             ; test09
             ; test10
             ; test11
             ; test12
             ; test13
             ; test14
             ; test15 ]
    ; "minimum", [ test16
                 ; test17
                 ; test18
                 ; test19
                 ; test20 ]
    ; "remove", [ test21
                ; test22
                ; test23
                ; test24
                ; test25
                ; test26
                ; test27
                ; test28 ]
    ; "iter", [ test29 ] ]
