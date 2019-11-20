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
             ; test09 ] ]
