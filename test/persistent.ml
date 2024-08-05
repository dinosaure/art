let () = Printexc.record_backtrace true

external random_seed : unit -> int array = "caml_sys_random_seed"

let seed = "4EygbdYh+v36vvrmD9YYP4byT5E3H7lTeXJiIj+dQnc="
let seed = Base64.decode_exn seed

let seed =
  let res = Array.make (String.length seed / 2) 0 in
  for i = 0 to (String.length seed / 2) - 1 do
    res.(i) <- (Char.code seed.[i * 2] lsl 8) lor Char.code seed.[(i * 2) + 1]
  done;
  res

let () =
  let random_seed = seed in
  Fmt.pr "Random: %a.\n%!" Fmt.(Dump.array int) random_seed;
  Random.full_init random_seed

let state_of_path path =
  let state, () = Part.(run closed (open_index writer ~path)) in
  state

let random_index : (_ Part.state, _) result Lazy.t =
  Lazy.from_fun @@ fun () ->
  let open Rresult in
  Bos.OS.File.tmp "index-%s" >>= fun path ->
  let path = Fpath.to_string path in
  match Part.(run closed (create path)) with
  | state, Ok () ->
      let state, () = Part.(run state (open_index writer ~path)) in
      Ok state
  | _closed, Error err -> Error err

let state_of_optional_path = function
  | Some path -> state_of_path path
  | None -> Rresult.R.failwith_error_msg (Lazy.force random_index)

let insert state key value =
  match Part.(run state (insert key value)) with
  | state, Ok () -> Ok state
  | state, Error `Already_exists -> Error (`Duplicate state)

let find state key =
  match Part.(run state (find key)) with
  | state, value -> Ok (state, value)
  | exception Not_found -> Error `Not_found

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a][%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Blue (fmt "%10d"))
        (Unix.getpid ())
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt
  in
  { Logs.report }

let setup_logs style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (reporter Fmt.stderr)

let () = setup_logs (Some `Ansi_tty) (Some Logs.Debug)

let test01 =
  Alcotest.test_case "test01" `Quick @@ fun path ->
  let state = state_of_optional_path path in
  let th0 =
    let open Part in
    let* _ = insert (Rowex.key "abc") 1 in
    let* v' = find (Rowex.key "abc") in
    Alcotest.(check int) "abc" v' 1;
    let* _ = insert (Rowex.key "ab") 2 in
    let* v' = find (Rowex.key "ab") in
    Alcotest.(check int) "ab" v' 2;
    let* _ = insert (Rowex.key "abcde") 3 in
    let* v0 = find (Rowex.key "abc") in
    let* v1 = find (Rowex.key "ab") in
    let* v2 = find (Rowex.key "abcde") in
    Alcotest.(check int) "abc" v0 1;
    Alcotest.(check int) "ab" v1 2;
    Alcotest.(check int) "abcde" v2 3;
    return ()
  in
  match Part.run state th0 with
  | _not_closed, () -> ()
  | exception Rowex.Duplicate ->
      Alcotest.failf "Insert a duplicate into the index"
  | exception Not_found -> Alcotest.failf "Key not found"

let test02 =
  Alcotest.test_case "test02" `Quick @@ fun path ->
  let state = state_of_optional_path path in
  let th0 =
    let open Part in
    let* _ = insert (Rowex.key "a0") 0 in
    let* _ = insert (Rowex.key "a1") 1 in
    let* _ = insert (Rowex.key "a2") 2 in
    let* _ = insert (Rowex.key "a3") 3 in
    let* v0 = find (Rowex.key "a0") in
    let* v1 = find (Rowex.key "a1") in
    let* v2 = find (Rowex.key "a2") in
    let* v3 = find (Rowex.key "a3") in
    Alcotest.(check int) "a0" v0 0;
    Alcotest.(check int) "a1" v1 1;
    Alcotest.(check int) "a2" v2 2;
    Alcotest.(check int) "a3" v3 3;
    let* _ = insert (Rowex.key "a4") 4 in
    let* v0 = find (Rowex.key "a0") in
    let* v1 = find (Rowex.key "a1") in
    let* v2 = find (Rowex.key "a2") in
    let* v3 = find (Rowex.key "a3") in
    let* v4 = find (Rowex.key "a4") in
    Alcotest.(check int) "a0" v0 0;
    Alcotest.(check int) "a1" v1 1;
    Alcotest.(check int) "a2" v2 2;
    Alcotest.(check int) "a3" v3 3;
    Alcotest.(check int) "a4" v4 4;
    return ()
  in
  match Part.run state th0 with
  | _not_closed, () -> ()
  | exception Rowex.Duplicate ->
      Alcotest.failf "Insert a duplicate into the index"
  | exception Not_found -> Alcotest.failf "Key not found"

let random_string len =
  let res = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set res i (Char.chr (1 + Random.int 255))
  done;
  Bytes.unsafe_to_string res

let test03 =
  Alcotest.test_case "test03" `Quick @@ fun path ->
  let max = 500 in
  let state = state_of_optional_path path in
  let vs =
    List.init max (fun _ -> (random_string (1 + Random.int 63), Random.int max))
  in
  let th0 =
    let open Part in
    let rec go0 = function
      | [] -> return ()
      | (k, v) :: r ->
          let* _ = Part.insert (Rowex.key k) v in
          Alcotest.(check pass) (Fmt.str "insert %S" k) () ();
          go0 r
    in
    let* () = go0 vs in
    let rec go1 = function
      | [] -> return ()
      | (k, v) :: r ->
          let* v' = find (Rowex.key k) in
          Alcotest.(check int) (Fmt.str "%S" k) v' v;
          go1 r
    in
    go1 vs
  in
  match Part.run state th0 with
  | _not_closed, () -> ()
  | exception Rowex.Duplicate ->
      Alcotest.failf "Insert a duplicate into the index"
  | exception Not_found -> Alcotest.failf "Key not found"

let test04 =
  Alcotest.test_case "test04" `Quick @@ fun path ->
  let state = state_of_optional_path path in
  let th0 =
    let open Part in
    let* _ = insert (Rowex.key "stone@meekness.com") 0 in
    let* _ = insert (Rowex.key "ca-tech@dps,centrin.net.id") 1 in
    let* _ = insert (Rowex.key "trinanda_lestyowati@elkomsel.co.id") 2 in
    let* _ = insert (Rowex.key "asst_dos@asonrasuna.com") 3 in
    let* _ = insert (Rowex.key "amartabali@dps.centrim.net.id") 4 in
    let* _ = insert (Rowex.key "achatv@cbn.net.id") 5 in
    let* _ = insert (Rowex.key "bali@tuguhotels.com") 6 in
    let* _ = insert (Rowex.key "baliminimalist@yahoo.com") 7 in
    (* prefix with [li] on a n16 node *)
    let* v0 = find (Rowex.key "bali@tuguhotels.com") in
    let* v1 = find (Rowex.key "baliminimalist@yahoo.com") in
    Alcotest.(check int) "bali@tuguhotels.com" v0 6;
    Alcotest.(check int) "baliminimalist@yahoo.com" v1 7;
    return ()
  in
  match Part.run state th0 with
  | _not_closed, () -> ()
  | exception Rowex.Duplicate ->
      Alcotest.failf "Insert a duplicate into the index"
  | exception Not_found -> Alcotest.failf "Key not found"

let test05 =
  Alcotest.test_case "test05" `Quick @@ fun path ->
  let state = state_of_optional_path path in
  let th0 =
    let open Part in
    let* _ = insert (Rowex.key "bliss@thebale.com") 8 in
    let* v0 = find (Rowex.key "bali@tuguhotels.com") in
    let* v1 = find (Rowex.key "baliminimalist@yahoo.com") in
    let* v2 = find (Rowex.key "bliss@thebale.com") in
    Alcotest.(check int) "bali@tuguhotels.com" v0 6;
    Alcotest.(check int) "baliminimalist@yahoo.com" v1 7;
    Alcotest.(check int) "bliss@thebale.com" v2 8;
    return ()
  in
  match Part.run state th0 with
  | _not_closed, () -> ()
  | exception Rowex.Duplicate ->
      Alcotest.failf "Insert a duplicate into the index"
  | exception Not_found -> Alcotest.failf "Key not found"

let test06 =
  Alcotest.test_case "test06" `Quick @@ fun path ->
  let state = state_of_optional_path path in
  let elts =
    [
      "adhidharma@denpasar.wasantara.net.id";
      "centralreservation@ramayanahotel.com";
      "apribadi@balimandira.com";
      "cdagenhart@ifc.org";
      "dana_supriyanto@interconti.com";
      "dos@novotelbali.com";
      "daniel@hotelpadma.com";
      "daniel@balibless.com";
      "djoko_p@jayakartahotelsresorts.com";
      "expdepot@indosat.net.id";
      "feby.adamsyah@idn.xerox.com";
      "christian_rizal@interconti.com";
      "singgih93@mailcity.com";
      "idonk_gebhoy@yahoo.com";
      "info@houseofbali.com";
      "kyohana@toureast.net";
      "sales@nusaduahotel.com";
      "jayakarta@mataram.wasantara.net.id";
      "mapindo@indo.net.id";
      "sm@ramayanahotel.com";
      "anekabeach@dps.centrin.net.id";
      "yogya@jayakartahotelsresorts.com";
      "garudawisatajaya@indo.net.id";
      "ketut@kbatur.com";
      "bondps@bonansatours.com";
      "witamgr@dps.centrin.net.id";
      "dtedja@indosat.net.id";
      "info@stpbali.ac.id";
      "baliprestigeho@dps.centrin.net.id";
      "pamilu@mas-travel.com";
      "amandabl@indosat.net.id";
      "marketing@csdwholiday.com";
      "luha89@yahoo.com";
      "indahsuluh2002@yahoo.com.sg";
      "imz1991@yahoo.com";
      "gus_war81@yahoo.com";
      "kf034@indosat.net.id";
      "800produkwil@posindonesia.co.id";
      "kontak.synergi@yahoo.com";
      "oekaoeka@yahoo.com";
      "fitrianti@hotmail.com";
      "meylina310@yahoo.com";
      "h4ntoro@yahoo.com";
      "novi_enbe@yahoo.com";
      "dila_dewata@yahoo.co.id";
      "tiena_asfary@yahoo.co.id";
      "da_lawoffice@yahoo.com";
      "rini@ncsecurities.biz";
      "sudarnoto_hakim@yahoo.com";
      "wastioke@yahoo.com";
      "leebahri@yahoo.com.";
      "lia_kiara97@yahoo.com";
      "rido@weddingku.com";
      "b_astuti@telkomsel.co.id";
    ]
  in
  let th0 =
    let open Part in
    let rec go0 idx = function
      | [] -> return ()
      | key :: r ->
          let* _ = insert (Rowex.key key) idx in
          go0 (succ idx) r
    in
    let* () = go0 0 elts in
    let* _ = insert (Rowex.key "garudawisata@indo.net.id") (-1) in
    let* v0 = find (Rowex.key "garudawisatajaya@indo.net.id") in
    let* v1 = find (Rowex.key "garudawisata@indo.net.id") in
    Alcotest.(check int) "garudawisatajaya@indo.net.id" v0 22;
    Alcotest.(check int) "garudawisata@indo.net.id" v1 (-1);
    let rec go1 idx = function
      | [] -> return ()
      | key :: r ->
          let* v' = find (Rowex.key key) in
          Alcotest.(check int) key idx v';
          go1 (succ idx) r
    in
    go1 0 elts
  in
  match Part.run state th0 with
  | _not_closed, () -> ()
  | exception Rowex.Duplicate ->
      Alcotest.failf "Insert a duplicate into the index"
  | exception Not_found -> Alcotest.failf "Key not found"

let test07 =
  Alcotest.test_case "test07" `Quick @@ fun _file ->
  let path = Rresult.R.get_ok (Bos.OS.File.tmp "index-%s") in
  let elts =
    [
      "adhidharma@denpasar.wasantara.net.id";
      "centralreservation@ramayanahotel.com";
      "apribadi@balimandira.com";
      "cdagenhart@ifc.org";
      "dana_supriyanto@interconti.com";
      "dos@novotelbali.com";
      "daniel@hotelpadma.com";
      "daniel@balibless.com";
      "djoko_p@jayakartahotelsresorts.com";
      "expdepot@indosat.net.id";
      "feby.adamsyah@idn.xerox.com";
      "christian_rizal@interconti.com";
      "singgih93@mailcity.com";
      "idonk_gebhoy@yahoo.com";
      "info@houseofbali.com";
      "kyohana@toureast.net";
      "sales@nusaduahotel.com";
      "jayakarta@mataram.wasantara.net.id";
      "mapindo@indo.net.id";
      "sm@ramayanahotel.com";
      "anekabeach@dps.centrin.net.id";
      "yogya@jayakartahotelsresorts.com";
      "garudawisatajaya@indo.net.id";
      "ketut@kbatur.com";
      "bondps@bonansatours.com";
      "witamgr@dps.centrin.net.id";
      "dtedja@indosat.net.id";
      "info@stpbali.ac.id";
      "baliprestigeho@dps.centrin.net.id";
      "pamilu@mas-travel.com";
      "amandabl@indosat.net.id";
      "marketing@csdwholiday.com";
      "luha89@yahoo.com";
      "indahsuluh2002@yahoo.com.sg";
      "imz1991@yahoo.com";
      "gus_war81@yahoo.com";
      "kf034@indosat.net.id";
      "800produkwil@posindonesia.co.id";
      "kontak.synergi@yahoo.com";
      "oekaoeka@yahoo.com";
      "fitrianti@hotmail.com";
      "meylina310@yahoo.com";
      "h4ntoro@yahoo.com";
      "novi_enbe@yahoo.com";
      "dila_dewata@yahoo.co.id";
      "tiena_asfary@yahoo.co.id";
      "da_lawoffice@yahoo.com";
      "rini@ncsecurities.biz";
      "sudarnoto_hakim@yahoo.com";
      "wastioke@yahoo.com";
      "leebahri@yahoo.com.";
      "lia_kiara97@yahoo.com";
      "rido@weddingku.com";
      "b_astuti@telkomsel.co.id";
      "garudawisata@indo.net.id";
      "grfurniture@yahoo.com";
      "gosyen2000@hotmail.com";
      "hvhfood@indosat.net.id";
      "hr@astonbali.com";
      "hary@wibisono-family.com";
      "fadlycak'p@yahoo.com";
      "ida_sampurniah@telkomsel.co.id";
      "muslim-pariwisata-bali@yahoogroups.com";
      "harisnira@yahoo.com";
      "sales@houseofbali.com";
      "baim_ron@yahoo.com";
      "ilhambali222@yahoo.com";
      "bungjon@gmail.com";
      "diar@bdg.centrin.net.id";
      "elmienruge@hotmail.com";
    ]
  in
  let th0 =
    let open Part in
    let* res = create (Fpath.to_string path) in
    match res with
    | Error (`Msg err) -> Alcotest.failf "%s." err
    | Ok () ->
        let* () = open_index writer ~path:(Fpath.to_string path) in
        let rec go0 idx = function
          | [] -> return ()
          | key :: r ->
              let* _ = insert (Rowex.key key) idx in
              go0 (succ idx) r
        in
        let* () = go0 0 elts in
        let* _ = insert (Rowex.key "galaxygarden2006@yahoo.com") (-1) in
        let rec go1 idx = function
          | [] -> close
          | key :: r ->
              let* v' = find (Rowex.key key) in
              Alcotest.(check int) key v' idx;
              go1 (succ idx) r
        in
        go1 0 elts
  in
  match Part.(run closed th0) with
  | _closed, () -> ()
  | exception Rowex.Duplicate ->
      Alcotest.failf "Insert a duplicate into the index"
  | exception Not_found -> Alcotest.failf "Key not found"

let test08 =
  Alcotest.test_case "test08" `Quick @@ fun _path ->
  let path = Rresult.R.get_ok (Bos.OS.File.tmp "index-%s") in
  let elts =
    [
      "adhidharma@denpasar.wasantara.net.id";
      "centralreservation@ramayanahotel.com";
      "apribadi@balimandira.com";
      "cdagenhart@ifc.org";
      "dana_supriyanto@interconti.com";
      "dos@novotelbali.com";
      "daniel@hotelpadma.com";
      "daniel@balibless.com";
      "djoko_p@jayakartahotelsresorts.com";
      "expdepot@indosat.net.id";
      "feby.adamsyah@idn.xerox.com";
      "christian_rizal@interconti.com";
      "singgih93@mailcity.com";
      "idonk_gebhoy@yahoo.com";
      "info@houseofbali.com";
      "kyohana@toureast.net";
      "sales@nusaduahotel.com";
      "jayakarta@mataram.wasantara.net.id";
      "mapindo@indo.net.id";
      "sm@ramayanahotel.com";
      "anekabeach@dps.centrin.net.id";
      "yogya@jayakartahotelsresorts.com";
      "garudawisatajaya@indo.net.id";
      "ketut@kbatur.com";
      "bondps@bonansatours.com";
      "witamgr@dps.centrin.net.id";
      "dtedja@indosat.net.id";
      "info@stpbali.ac.id";
      "baliprestigeho@dps.centrin.net.id";
      "pamilu@mas-travel.com";
      "amandabl@indosat.net.id";
      "marketing@csdwholiday.com";
      "luha89@yahoo.com";
      "indahsuluh2002@yahoo.com.sg";
      "imz1991@yahoo.com";
      "gus_war81@yahoo.com";
      "kf034@indosat.net.id";
      "800produkwil@posindonesia.co.id";
      "kontak.synergi@yahoo.com";
      "oekaoeka@yahoo.com";
      "fitrianti@hotmail.com";
      "meylina310@yahoo.com";
      "h4ntoro@yahoo.com";
      "novi_enbe@yahoo.com";
      "dila_dewata@yahoo.co.id";
      "tiena_asfary@yahoo.co.id";
      "da_lawoffice@yahoo.com";
      "rini@ncsecurities.biz";
      "sudarnoto_hakim@yahoo.com";
      "wastioke@yahoo.com";
      "leebahri@yahoo.com.";
      "lia_kiara97@yahoo.com";
      "rido@weddingku.com";
      "b_astuti@telkomsel.co.id";
      "garudawisata@indo.net.id";
      "grfurniture@yahoo.com";
      "gosyen2000@hotmail.com";
      "hvhfood@indosat.net.id";
      "hr@astonbali.com";
      "hary@wibisono-family.com";
      "fadlycak'p@yahoo.com";
      "ida_sampurniah@telkomsel.co.id";
      "muslim-pariwisata-bali@yahoogroups.com";
      "harisnira@yahoo.com";
      "sales@houseofbali.com";
      "baim_ron@yahoo.com";
      "ilhambali222@yahoo.com";
      "bungjon@gmail.com";
      "diar@bdg.centrin.net.id";
      "elmienruge@hotmail.com";
      "galaxygarden2006@yahoo.com";
      "gorisata@indosat.net.id";
      "maulitasarihani@yahoo.com";
      "hamiluddakwah@gmail.com.au";
      "bounty@indo.net.id,";
      "michi@ritzcarlton-bali.com,";
      "orridor@dps.centrin.net.id,";
      "ngumina@hotmail.com,";
      "made@mas-travel.com,";
      "evi@mas-travel.com,";
      "wibawa@mas-travel.com,";
      "saihubaly@yahoo.co.id,";
      "swa_candra@yahoo.com,";
      "picapica@denpasar.wasantara.net.id,";
      "griyasantrian@santrian.com,";
      "yuni6671@gmail.com,";
      "phbalichef@indo.net.id,";
      "vendra@keratonjimbaranresort.com,";
      "bali@pansea.com,";
      "sales@legianbeachbali.com,";
      "purchasing@meliabali.com,";
      "swacandra@telkom.net,";
      "lysbeth@paintballbali.com,";
      "trvlindo@upg.mega.net.id,";
      "lim_thefaith@yahoo.com,";
      "uungtb@yahoo.com.au,";
      "vivaldil307@hotmail.com,";
      "iodakon@yahoo.co.id,";
      "reservation@pendawahotel.com,";
      "ptbon@dps.centrin.net.id,";
      "ptlamak@indosat.net.id,";
      "sculpt@indo.net.id,";
      "memedi-gwkbali@dps.centrin.net.id,";
      "info@leisuredream.com,";
      "indra_wijaya@hero.co.id,";
      "ndbconvex@bagus-discovery.com,";
      "Endro@bma-merdeka.com,";
      "wsuardana@indosat.net.id,";
      "bali@esmirada.com,";
      "BAL.Purchasing@fourseasons.com,";
      "ruby@marthatilaar-spa.com,";
      "villaseminyak@eksadata.com,";
      "sariati@sanurbeach.aerowisata.com,";
      "info@jenggala-bali.com,";
      "chef@nusaduahotel.com,";
      "info@balicateringcompany.com,";
      "moka@dps.mega.net.id,";
      "zsa@eyeview.info,";
      "winarios@indosat.net.id,";
      "project@balihai-rsort.com,";
      "vivi@kopibali.com,";
      "peninsulabali@dps.centrin.net.id,";
      "ust.july@mas-travel.com,";
      "ubud@pansea.com,";
      "ustad_july@yahoo.com,";
      "thebarbali@hotmail.com,";
      "trustbali@balidream.com,";
      "teraoka@his-bali.com,";
      "candle@dps.centrin.net.id,";
      "waterbom@denpasar.wasantara.net.id,";
      "ib.suparsa@yahoo.com,";
      "budhipra@nesiancea.com,";
      "info@kindvillabintang.com,";
      "pch@novotelbali.com,";
      "parigata@indosat.net.id,";
      "mail@grandmirage.com,";
      "ananda_resort@hotmail.com,";
      "info@risatabali.com,";
      "gwkbali@indosat.net.id,";
      "rai@gosharestaurant.com,";
      "santika@santikabali.com,";
      "sahidbl@indosat.net.id,";
      "tubanrestaurant@yahoo.com,";
      "sales@thejimbaranbali.com,";
      "info@thejimbaranbali.com,";
      "sari@bubbagumpbali.com,";
      "Winnie@grandlingga.com,";
      "juaidy_asia@yahoo.com,";
      "vicmgr@i-xplore.com,";
      "langka@theclubstore.co.id,";
      "lilakresna@ConradBali.com,";
      "wayan.atmaja@luxurycollecton.com,";
      "Cisabali@indo.net.id,";
      "garrant@indo.net.id,";
      "owenwister@yahoo.com,";
      "tiara@dps.mega.net.id,";
      "info@nzmuslim.net,";
      "yuanito.kurniawan@sea.ccamatil.com,";
      "pitamaha@indosat.net.id,";
      "yunani@theclubstore.co.id,";
      "deklis@hotmail.com,";
      "cianjur@indo.net.id,";
      "mahajayatower@hotmail.com,";
      "endra@centrin.net.id,";
      "wayan.dirayana@fourseasons.com,";
      "balinaga@dps.centrin.net.id,";
      "tiaradwt@dps.centrin.net.id,";
      "candrator@hotmail.com,";
      "altaraspa@yahoo.com,";
      "fani@clubbali.com,";
      "Itudm@dps.centrin.net.id,";
      "baliratuspa@biz.net.id,";
      "kawasspa@indosat.net.id,";
      "hatoe7@yahoo.co.jp,";
      "sales@mimpi.com,";
      "theroyal@indosat.net.id,";
      "chakra_92@yahoo.com,";
      "u_dmtdps@sosro.com,";
      "januar@citramedia.net,";
      "januar@balivisioncomp.com,";
      "admin@balivisioncomp.com,";
      "ansri@dps.mega.net.id,";
      "info@rijasaresort-villas.com,";
      "sales@komaneka.com,";
      "multigun@indo.net.id,";
      "ishwari@bagus-discovery.com,";
      "utami@bali-exoticwedding.com,";
      "putra_wirata@hotmail.com,";
      "arte@dps.centrin.net.id,";
      "hamiludd2kwah@yahoo.com.au,";
      "btu_cipluk@yahoo.com,";
      "agus@indo-journey.com,";
      "agus.winarko@gmail.com,";
      "agus.amirudin@wilmar.co.id,";
      "adamsilver@lycos.com,";
      "yayasanlaroyba@yahoo.co.id,";
      "luminaABC@hotmail.com,";
      "umasapna@coconuthomes.com,";
      "udsupradinasty@yahoo.co.id,";
      "ticketing@bagus-discovery.com,";
      "tejo@pttropical.co.id,";
      "syamklw@yahoo.com,";
      "sutiarso21@yahoo.com,";
      "silvia_maniz@yahoo.com,";
      "yenny_kurniawaty@telkomsel.co.id,";
      "lega@kramatdjatigroup.com,";
      "stadiumcafe@indonet.id,";
      "agencyfreestylebali@yahoo.com,";
      "yayaqdarma@yahoo.co.id,";
      "hanafiid@yahoo.com,";
      "ricky_dvt@yahoo.co.id,";
      "teuku_umar@binus-centre.com,";
      "flp_bali@yahoo.com,";
      "andy@ritzcarlton-bali.com,";
      "bapakbakery@dps.centrin.net.id,";
      "siddiq@teacher.com,";
      "clipper@indo.net.id,";
      "puricendana@yahoo.com,";
      "info@ripcurlschoolsurf.com,";
      "sales@ramabeachhotel.com,";
      "healing@indosat.net.id,";
      "djinaldi@yahoo.co.uk,";
      "rotary.bali.kuta@gmail.com,";
      "dadang@ma-joly.com,";
      "takenoko_bali@yahoo.co.id,";
      "hrd@novotelbali.com,";
      "purwa@kcb-tours.com,";
      "anggie.gendut@england.com,";
      "novyog@indo.net.id,";
      "reservation@meliabali.com,";
      "sales@meliabali.com,";
      "info@rkeconsulting.com,";
      "andisetiaji@abacus-ind.co.id,";
      "sales.corp@swissgrandbali.com,";
      "karsana.wirajaya@trac.astra.co.id,";
      "muliatr@indosat.net.id,";
      "nita@surfer-girl.com,";
      "diah.permana@bagus-discovery.com,";
      "purwabali@yahoo.com,";
      "oly@islandconcpets.com,";
      "info@islandconcepts.com,";
      "gag@indo.net.id,";
      "gkumala@indosat.net.id,";
      "thegardeniavillas@meliabali.com,";
      "purchasing.mgr@thelegianbali.com,";
      "info@paradisebaliholidays.com,";
      "agus.winarko@bagus-discovery.com,";
      "cozytimes26@yahoo.com,";
      "info@papua-adventures.com,";
      "lokasaribali@hotmail.com,";
      "wahana@baliforyou.com,";
      "Stephen@victuslife.com,";
      "operations@atlasbalitours.com,";
      "balicoffeeshop@hotmail.com,";
      "mayakutacentre@telkom.net,";
      "rikmawan@dps.centrin.net.id,";
      "ndbt@bagus-discovery.com,";
      "info@indographs.com,";
      "aridwan_sgb@yahoo.com,";
      "bali@atmosphere.co.id,";
      "plmgrd@indosat.net.id,";
      "balibless@padmaubud.biz,";
      "baliaura@yahoo.com,";
      "andalan@bali.net,";
      "dmandiri@indo.net.id,";
      "pernadi@rad.net.id,";
      "Tabetha@BeyondMenus.com,";
      "adityafood@yahoo.com,";
      "sarana_com@yahoo.com,";
      "pasadena@chek.com,";
    ]
  in
  let th0 =
    let open Part in
    let* res = create (Fpath.to_string path) in
    match res with
    | Error (`Msg err) -> Alcotest.failf "%s." err
    | Ok () ->
        let* () = open_index writer ~path:(Fpath.to_string path) in
        let rec go0 idx = function
          | [] -> return ()
          | key :: r ->
              let* _ = insert (Rowex.key key) idx in
              go0 (succ idx) r
        in
        let* () = go0 0 elts in
        let* _ = insert (Rowex.key "sales@pica-pica.com,") (-1) in
        let* v' = find (Rowex.key "sales@pica-pica.com,") in
        Alcotest.(check int) "sales@pica-pica.com," v' (-1);
        let rec go1 idx = function
          | [] -> close
          | key :: r ->
              let* v' = find (Rowex.key key) in
              Alcotest.(check int) key v' idx;
              go1 (succ idx) r
        in
        go1 0 elts
  in
  match Part.(run closed th0) with
  | _closed, () -> ()
  | exception Rowex.Duplicate ->
      Alcotest.failf "Insert a duplicate into the index"
  | exception Not_found -> Alcotest.failf "Key not found"

let test09 =
  Alcotest.test_case "test09" `Quick @@ fun _path ->
  let path = Rresult.R.get_ok (Bos.OS.File.tmp "index-%s") in
  let elts =
    [
      "adhidharma@denpasar.wasantara.net.id";
      "centralreservation@ramayanahotel.com";
      "apribadi@balimandira.com";
      "cdagenhart@ifc.org";
      "dana_supriyanto@interconti.com";
      "dos@novotelbali.com";
      "daniel@hotelpadma.com";
      "daniel@balibless.com";
      "djoko_p@jayakartahotelsresorts.com";
      "expdepot@indosat.net.id";
      "feby.adamsyah@idn.xerox.com";
      "christian_rizal@interconti.com";
      "singgih93@mailcity.com";
      "idonk_gebhoy@yahoo.com";
      "info@houseofbali.com";
      "kyohana@toureast.net";
      "sales@nusaduahotel.com";
      "jayakarta@mataram.wasantara.net.id";
      "mapindo@indo.net.id";
      "sm@ramayanahotel.com";
      "anekabeach@dps.centrin.net.id";
      "yogya@jayakartahotelsresorts.com";
      "garudawisatajaya@indo.net.id";
      "ketut@kbatur.com";
      "bondps@bonansatours.com";
      "witamgr@dps.centrin.net.id";
      "dtedja@indosat.net.id";
      "info@stpbali.ac.id";
      "baliprestigeho@dps.centrin.net.id";
      "pamilu@mas-travel.com";
      "amandabl@indosat.net.id";
      "marketing@csdwholiday.com";
      "luha89@yahoo.com";
      "indahsuluh2002@yahoo.com.sg";
      "imz1991@yahoo.com";
      "gus_war81@yahoo.com";
      "kf034@indosat.net.id";
      "800produkwil@posindonesia.co.id";
      "kontak.synergi@yahoo.com";
      "oekaoeka@yahoo.com";
      "fitrianti@hotmail.com";
      "meylina310@yahoo.com";
      "h4ntoro@yahoo.com";
      "novi_enbe@yahoo.com";
      "dila_dewata@yahoo.co.id";
      "tiena_asfary@yahoo.co.id";
      "da_lawoffice@yahoo.com";
      "rini@ncsecurities.biz";
      "sudarnoto_hakim@yahoo.com";
      "wastioke@yahoo.com";
      "leebahri@yahoo.com.";
      "lia_kiara97@yahoo.com";
      "rido@weddingku.com";
      "b_astuti@telkomsel.co.id";
      "garudawisata@indo.net.id";
      "grfurniture@yahoo.com";
      "gosyen2000@hotmail.com";
      "hvhfood@indosat.net.id";
      "hr@astonbali.com";
      "hary@wibisono-family.com";
      "fadlycak'p@yahoo.com";
      "ida_sampurniah@telkomsel.co.id";
      "muslim-pariwisata-bali@yahoogroups.com";
      "harisnira@yahoo.com";
      "sales@houseofbali.com";
      "baim_ron@yahoo.com";
      "ilhambali222@yahoo.com";
      "bungjon@gmail.com";
      "diar@bdg.centrin.net.id";
      "elmienruge@hotmail.com";
      "galaxygarden2006@yahoo.com";
      "gorisata@indosat.net.id";
      "maulitasarihani@yahoo.com";
      "hamiluddakwah@gmail.com.au";
      "bounty@indo.net.id,";
      "michi@ritzcarlton-bali.com,";
      "orridor@dps.centrin.net.id,";
      "ngumina@hotmail.com,";
      "made@mas-travel.com,";
      "evi@mas-travel.com,";
      "wibawa@mas-travel.com,";
      "saihubaly@yahoo.co.id,";
      "swa_candra@yahoo.com,";
      "picapica@denpasar.wasantara.net.id,";
      "griyasantrian@santrian.com,";
      "yuni6671@gmail.com,";
      "phbalichef@indo.net.id,";
      "vendra@keratonjimbaranresort.com,";
      "bali@pansea.com,";
      "sales@legianbeachbali.com,";
      "purchasing@meliabali.com,";
      "swacandra@telkom.net,";
      "lysbeth@paintballbali.com,";
      "trvlindo@upg.mega.net.id,";
      "lim_thefaith@yahoo.com,";
      "uungtb@yahoo.com.au,";
      "vivaldil307@hotmail.com,";
      "iodakon@yahoo.co.id,";
      "reservation@pendawahotel.com,";
      "ptbon@dps.centrin.net.id,";
      "ptlamak@indosat.net.id,";
      "sculpt@indo.net.id,";
      "memedi-gwkbali@dps.centrin.net.id,";
      "info@leisuredream.com,";
      "indra_wijaya@hero.co.id,";
      "ndbconvex@bagus-discovery.com,";
      "Endro@bma-merdeka.com,";
      "wsuardana@indosat.net.id,";
      "bali@esmirada.com,";
      "BAL.Purchasing@fourseasons.com,";
      "ruby@marthatilaar-spa.com,";
      "villaseminyak@eksadata.com,";
      "sariati@sanurbeach.aerowisata.com,";
      "info@jenggala-bali.com,";
      "chef@nusaduahotel.com,";
      "info@balicateringcompany.com,";
      "moka@dps.mega.net.id,";
      "zsa@eyeview.info,";
      "winarios@indosat.net.id,";
      "project@balihai-rsort.com,";
      "vivi@kopibali.com,";
      "peninsulabali@dps.centrin.net.id,";
      "ust.july@mas-travel.com,";
      "ubud@pansea.com,";
      "ustad_july@yahoo.com,";
      "thebarbali@hotmail.com,";
      "trustbali@balidream.com,";
      "teraoka@his-bali.com,";
      "candle@dps.centrin.net.id,";
      "waterbom@denpasar.wasantara.net.id,";
      "ib.suparsa@yahoo.com,";
      "budhipra@nesiancea.com,";
      "info@kindvillabintang.com,";
      "pch@novotelbali.com,";
      "parigata@indosat.net.id,";
      "mail@grandmirage.com,";
      "ananda_resort@hotmail.com,";
      "info@risatabali.com,";
      "gwkbali@indosat.net.id,";
      "rai@gosharestaurant.com,";
      "santika@santikabali.com,";
      "sahidbl@indosat.net.id,";
      "tubanrestaurant@yahoo.com,";
      "sales@thejimbaranbali.com,";
      "info@thejimbaranbali.com,";
      "sari@bubbagumpbali.com,";
      "Winnie@grandlingga.com,";
      "juaidy_asia@yahoo.com,";
      "vicmgr@i-xplore.com,";
      "langka@theclubstore.co.id,";
      "lilakresna@ConradBali.com,";
      "wayan.atmaja@luxurycollecton.com,";
      "Cisabali@indo.net.id,";
      "garrant@indo.net.id,";
      "owenwister@yahoo.com,";
      "tiara@dps.mega.net.id,";
      "info@nzmuslim.net,";
      "yuanito.kurniawan@sea.ccamatil.com,";
      "pitamaha@indosat.net.id,";
      "yunani@theclubstore.co.id,";
      "deklis@hotmail.com,";
      "cianjur@indo.net.id,";
      "mahajayatower@hotmail.com,";
      "endra@centrin.net.id,";
      "wayan.dirayana@fourseasons.com,";
      "balinaga@dps.centrin.net.id,";
      "tiaradwt@dps.centrin.net.id,";
      "candrator@hotmail.com,";
      "altaraspa@yahoo.com,";
      "fani@clubbali.com,";
      "Itudm@dps.centrin.net.id,";
      "baliratuspa@biz.net.id,";
      "kawasspa@indosat.net.id,";
      "hatoe7@yahoo.co.jp,";
      "sales@mimpi.com,";
      "theroyal@indosat.net.id,";
      "chakra_92@yahoo.com,";
      "u_dmtdps@sosro.com,";
      "januar@citramedia.net,";
      "januar@balivisioncomp.com,";
      "admin@balivisioncomp.com,";
      "ansri@dps.mega.net.id,";
      "info@rijasaresort-villas.com,";
      "sales@komaneka.com,";
      "multigun@indo.net.id,";
      "ishwari@bagus-discovery.com,";
      "utami@bali-exoticwedding.com,";
      "putra_wirata@hotmail.com,";
      "arte@dps.centrin.net.id,";
      "hamiludd2kwah@yahoo.com.au,";
      "btu_cipluk@yahoo.com,";
      "agus@indo-journey.com,";
      "agus.winarko@gmail.com,";
      "agus.amirudin@wilmar.co.id,";
      "adamsilver@lycos.com,";
      "yayasanlaroyba@yahoo.co.id,";
      "luminaABC@hotmail.com,";
      "umasapna@coconuthomes.com,";
      "udsupradinasty@yahoo.co.id,";
      "ticketing@bagus-discovery.com,";
      "tejo@pttropical.co.id,";
      "syamklw@yahoo.com,";
      "sutiarso21@yahoo.com,";
      "silvia_maniz@yahoo.com,";
      "yenny_kurniawaty@telkomsel.co.id,";
      "lega@kramatdjatigroup.com,";
      "stadiumcafe@indonet.id,";
      "agencyfreestylebali@yahoo.com,";
      "yayaqdarma@yahoo.co.id,";
      "hanafiid@yahoo.com,";
      "ricky_dvt@yahoo.co.id,";
      "teuku_umar@binus-centre.com,";
      "flp_bali@yahoo.com,";
      "andy@ritzcarlton-bali.com,";
      "bapakbakery@dps.centrin.net.id,";
      "siddiq@teacher.com,";
      "clipper@indo.net.id,";
      "puricendana@yahoo.com,";
      "info@ripcurlschoolsurf.com,";
      "sales@ramabeachhotel.com,";
      "healing@indosat.net.id,";
      "djinaldi@yahoo.co.uk,";
      "rotary.bali.kuta@gmail.com,";
      "dadang@ma-joly.com,";
      "takenoko_bali@yahoo.co.id,";
      "hrd@novotelbali.com,";
      "purwa@kcb-tours.com,";
      "anggie.gendut@england.com,";
      "novyog@indo.net.id,";
      "reservation@meliabali.com,";
      "sales@meliabali.com,";
      "info@rkeconsulting.com,";
      "andisetiaji@abacus-ind.co.id,";
      "sales.corp@swissgrandbali.com,";
      "karsana.wirajaya@trac.astra.co.id,";
      "muliatr@indosat.net.id,";
      "nita@surfer-girl.com,";
      "diah.permana@bagus-discovery.com,";
      "purwabali@yahoo.com,";
      "oly@islandconcpets.com,";
      "info@islandconcepts.com,";
      "gag@indo.net.id,";
      "gkumala@indosat.net.id,";
      "thegardeniavillas@meliabali.com,";
      "purchasing.mgr@thelegianbali.com,";
      "info@paradisebaliholidays.com,";
      "agus.winarko@bagus-discovery.com,";
      "cozytimes26@yahoo.com,";
      "info@papua-adventures.com,";
      "lokasaribali@hotmail.com,";
      "wahana@baliforyou.com,";
      "Stephen@victuslife.com,";
      "operations@atlasbalitours.com,";
      "balicoffeeshop@hotmail.com,";
      "mayakutacentre@telkom.net,";
      "rikmawan@dps.centrin.net.id,";
      "ndbt@bagus-discovery.com,";
      "info@indographs.com,";
      "aridwan_sgb@yahoo.com,";
      "bali@atmosphere.co.id,";
      "plmgrd@indosat.net.id,";
      "balibless@padmaubud.biz,";
      "baliaura@yahoo.com,";
      "andalan@bali.net,";
      "dmandiri@indo.net.id,";
      "pernadi@rad.net.id,";
      "Tabetha@BeyondMenus.com,";
      "adityafood@yahoo.com,";
      "sarana_com@yahoo.com,";
      "pasadena@chek.com,";
      "sales@pica-pica.com,";
      "menara_fbi@hotmail.com,";
      "home_treasure@hotmail.com,";
      "aamsalim@dps.centrin.net.id,";
      "shell_enoproduction@yahoo.com,";
      "geckoleather@hotmail.com,";
      "milagro_bali@hotmail.com,";
      "gemini19id@yahoo.com,";
      "karyacargo@dps.centrin.net.id,";
      "darabali@indo.net.id,";
      "padiprada@hotmail.com,";
      "vijowiz@yahoo.com,";
      "cafejimbaran@mekarsaribali.com,";
      "isnamks@yahoo.com,";
      "sales@allseasonslegian.com,";
      "chitra@cangguclub.com,";
      "cheriaM@xl.co.id,";
      "geo-trek@dps.centrin.net.id,";
      "sales@balipasadena.com,";
      "sales@villahening.com,";
      "fc@novotelbali.com,";
      "maolbing83@yahoo.co.id,";
      "info@dimensitropika.com,";
      "news@tabloidpiknik.com,";
      "mediacentre@bali-tourism.com,";
      "bioland-bali@telkom.net,";
      "glf-bali@indo.net.id,";
      "info@asiabali.com,";
      "takanit@yahoo.com,";
      "jamal@hrbc-bali.co.id,";
      "naniek@alilahotels.com,";
      "ndbtdps@dps.mega.net.id,";
      "mbcbali_jaka@yahoo.com,";
      "masnyonya@telkom.net,";
      "merrystravel@dps.centrin.net.id,";
      "mail@baliintermedia.com,";
      "mitrakridamandiri@hotmail.com,";
      "kartikaplz@denpasar.wasantara.net.id,";
      "oedps@indosat.net.id,";
      "jalirest@indosat.net.id,";
      "jenni_hartatik@interconti.com,";
      "info@alamkulkul.com,";
      "info@aggacitta.com,";
      "info@jasatours.com,";
      "iskandar.Liemena@idn.xerox.com,";
      "info@lorinresortsababai.com,";
      "ketutsukarta@telkom.net,";
      "renata.hutasoit@hyattintl.com,";
      "sukiato@hotelpadma.com,";
      "salesser@idola.net.id,";
      "sales@bali-clubaqua.com,";
      "sales@amandaresort.com,";
      "sales@balimandira.com,";
    ]
  in
  let th0 =
    let open Part in
    let* res = create (Fpath.to_string path) in
    match res with
    | Error (`Msg err) -> Alcotest.failf "%s." err
    | Ok () ->
        let* () = open_index writer ~path:(Fpath.to_string path) in
        let rec go0 idx = function
          | [] -> return ()
          | key :: r ->
              let* _ = insert (Rowex.key key) idx in
              go0 (succ idx) r
        in
        let* () = go0 0 elts in
        let* _ =
          insert (Rowex.key "reservation@ramacandidasahotel.com,") (-1)
        in
        let* v' = find (Rowex.key "reservation@ramacandidasahotel.com,") in
        Alcotest.(check int) "reservation@ramacandidasahotel.com," v' (-1);
        let rec go1 idx = function
          | [] -> close
          | key :: r ->
              let* v' = find (Rowex.key key) in
              Alcotest.(check int) key v' idx;
              go1 (succ idx) r
        in
        go1 0 elts
  in
  match Part.(run closed th0) with
  | _closed, () -> ()
  | exception Rowex.Duplicate ->
      Alcotest.failf "Insert a duplicate into the index"
  | exception Not_found -> Alcotest.failf "Key not found"

let test10 =
  Alcotest.test_case "test10" `Quick @@ fun _path ->
  let path = Rresult.R.get_ok (Bos.OS.File.tmp "index-%s") in
  let elts =
    [
      "adhidharma@denpasar.wasantara.net.id";
      "centralreservation@ramayanahotel.com";
      "apribadi@balimandira.com";
      "cdagenhart@ifc.org";
      "dana_supriyanto@interconti.com";
      "dos@novotelbali.com";
      "daniel@hotelpadma.com";
      "daniel@balibless.com";
      "djoko_p@jayakartahotelsresorts.com";
      "expdepot@indosat.net.id";
      "feby.adamsyah@idn.xerox.com";
      "christian_rizal@interconti.com";
      "singgih93@mailcity.com";
      "idonk_gebhoy@yahoo.com";
      "info@houseofbali.com";
      "kyohana@toureast.net";
      "sales@nusaduahotel.com";
      "jayakarta@mataram.wasantara.net.id";
      "mapindo@indo.net.id";
      "sm@ramayanahotel.com";
      "anekabeach@dps.centrin.net.id";
      "yogya@jayakartahotelsresorts.com";
      "garudawisatajaya@indo.net.id";
      "ketut@kbatur.com";
      "bondps@bonansatours.com";
      "witamgr@dps.centrin.net.id";
      "dtedja@indosat.net.id";
      "info@stpbali.ac.id";
      "baliprestigeho@dps.centrin.net.id";
      "pamilu@mas-travel.com";
      "amandabl@indosat.net.id";
      "marketing@csdwholiday.com";
      "luha89@yahoo.com";
      "indahsuluh2002@yahoo.com.sg";
      "imz1991@yahoo.com";
      "gus_war81@yahoo.com";
      "kf034@indosat.net.id";
      "800produkwil@posindonesia.co.id";
      "kontak.synergi@yahoo.com";
      "oekaoeka@yahoo.com";
      "fitrianti@hotmail.com";
      "meylina310@yahoo.com";
      "h4ntoro@yahoo.com";
      "novi_enbe@yahoo.com";
      "dila_dewata@yahoo.co.id";
      "tiena_asfary@yahoo.co.id";
      "da_lawoffice@yahoo.com";
      "rini@ncsecurities.biz";
      "sudarnoto_hakim@yahoo.com";
      "wastioke@yahoo.com";
      "leebahri@yahoo.com.";
      "lia_kiara97@yahoo.com";
      "rido@weddingku.com";
      "b_astuti@telkomsel.co.id";
      "garudawisata@indo.net.id";
      "grfurniture@yahoo.com";
      "gosyen2000@hotmail.com";
      "hvhfood@indosat.net.id";
      "hr@astonbali.com";
      "hary@wibisono-family.com";
      "fadlycak'p@yahoo.com";
      "ida_sampurniah@telkomsel.co.id";
      "muslim-pariwisata-bali@yahoogroups.com";
      "harisnira@yahoo.com";
      "sales@houseofbali.com";
      "baim_ron@yahoo.com";
      "ilhambali222@yahoo.com";
      "bungjon@gmail.com";
      "diar@bdg.centrin.net.id";
      "elmienruge@hotmail.com";
      "galaxygarden2006@yahoo.com";
      "gorisata@indosat.net.id";
      "maulitasarihani@yahoo.com";
      "hamiluddakwah@gmail.com.au";
      "bounty@indo.net.id,";
      "michi@ritzcarlton-bali.com,";
      "orridor@dps.centrin.net.id,";
      "ngumina@hotmail.com,";
      "made@mas-travel.com,";
      "evi@mas-travel.com,";
      "wibawa@mas-travel.com,";
      "saihubaly@yahoo.co.id,";
      "swa_candra@yahoo.com,";
      "picapica@denpasar.wasantara.net.id,";
      "griyasantrian@santrian.com,";
      "yuni6671@gmail.com,";
      "phbalichef@indo.net.id,";
      "vendra@keratonjimbaranresort.com,";
      "bali@pansea.com,";
      "sales@legianbeachbali.com,";
      "purchasing@meliabali.com,";
      "swacandra@telkom.net,";
      "lysbeth@paintballbali.com,";
      "trvlindo@upg.mega.net.id,";
      "lim_thefaith@yahoo.com,";
      "uungtb@yahoo.com.au,";
      "vivaldil307@hotmail.com,";
      "iodakon@yahoo.co.id,";
      "reservation@pendawahotel.com,";
      "ptbon@dps.centrin.net.id,";
      "ptlamak@indosat.net.id,";
      "sculpt@indo.net.id,";
      "memedi-gwkbali@dps.centrin.net.id,";
      "info@leisuredream.com,";
      "indra_wijaya@hero.co.id,";
      "ndbconvex@bagus-discovery.com,";
      "Endro@bma-merdeka.com,";
      "wsuardana@indosat.net.id,";
      "bali@esmirada.com,";
      "BAL.Purchasing@fourseasons.com,";
      "ruby@marthatilaar-spa.com,";
      "villaseminyak@eksadata.com,";
      "sariati@sanurbeach.aerowisata.com,";
      "info@jenggala-bali.com,";
      "chef@nusaduahotel.com,";
      "info@balicateringcompany.com,";
      "moka@dps.mega.net.id,";
      "zsa@eyeview.info,";
      "winarios@indosat.net.id,";
      "project@balihai-rsort.com,";
      "vivi@kopibali.com,";
      "peninsulabali@dps.centrin.net.id,";
      "ust.july@mas-travel.com,";
      "ubud@pansea.com,";
      "ustad_july@yahoo.com,";
      "thebarbali@hotmail.com,";
      "trustbali@balidream.com,";
      "teraoka@his-bali.com,";
      "candle@dps.centrin.net.id,";
      "waterbom@denpasar.wasantara.net.id,";
      "ib.suparsa@yahoo.com,";
      "budhipra@nesiancea.com,";
      "info@kindvillabintang.com,";
      "pch@novotelbali.com,";
      "parigata@indosat.net.id,";
      "mail@grandmirage.com,";
      "ananda_resort@hotmail.com,";
      "info@risatabali.com,";
      "gwkbali@indosat.net.id,";
      "rai@gosharestaurant.com,";
      "santika@santikabali.com,";
      "sahidbl@indosat.net.id,";
      "tubanrestaurant@yahoo.com,";
      "sales@thejimbaranbali.com,";
      "info@thejimbaranbali.com,";
      "sari@bubbagumpbali.com,";
      "Winnie@grandlingga.com,";
      "juaidy_asia@yahoo.com,";
      "vicmgr@i-xplore.com,";
      "langka@theclubstore.co.id,";
      "lilakresna@ConradBali.com,";
      "wayan.atmaja@luxurycollecton.com,";
      "Cisabali@indo.net.id,";
      "garrant@indo.net.id,";
      "owenwister@yahoo.com,";
      "tiara@dps.mega.net.id,";
      "info@nzmuslim.net,";
      "yuanito.kurniawan@sea.ccamatil.com,";
      "pitamaha@indosat.net.id,";
      "yunani@theclubstore.co.id,";
      "deklis@hotmail.com,";
      "cianjur@indo.net.id,";
      "mahajayatower@hotmail.com,";
      "endra@centrin.net.id,";
      "wayan.dirayana@fourseasons.com,";
      "balinaga@dps.centrin.net.id,";
      "tiaradwt@dps.centrin.net.id,";
      "candrator@hotmail.com,";
      "altaraspa@yahoo.com,";
      "fani@clubbali.com,";
      "Itudm@dps.centrin.net.id,";
      "baliratuspa@biz.net.id,";
      "kawasspa@indosat.net.id,";
      "hatoe7@yahoo.co.jp,";
      "sales@mimpi.com,";
      "theroyal@indosat.net.id,";
      "chakra_92@yahoo.com,";
      "u_dmtdps@sosro.com,";
      "januar@citramedia.net,";
      "januar@balivisioncomp.com,";
      "admin@balivisioncomp.com,";
      "ansri@dps.mega.net.id,";
      "info@rijasaresort-villas.com,";
      "sales@komaneka.com,";
      "multigun@indo.net.id,";
      "ishwari@bagus-discovery.com,";
      "utami@bali-exoticwedding.com,";
      "putra_wirata@hotmail.com,";
      "arte@dps.centrin.net.id,";
      "hamiludd2kwah@yahoo.com.au,";
      "btu_cipluk@yahoo.com,";
      "agus@indo-journey.com,";
      "agus.winarko@gmail.com,";
      "agus.amirudin@wilmar.co.id,";
      "adamsilver@lycos.com,";
      "yayasanlaroyba@yahoo.co.id,";
      "luminaABC@hotmail.com,";
      "umasapna@coconuthomes.com,";
      "udsupradinasty@yahoo.co.id,";
      "ticketing@bagus-discovery.com,";
      "tejo@pttropical.co.id,";
      "syamklw@yahoo.com,";
      "sutiarso21@yahoo.com,";
      "silvia_maniz@yahoo.com,";
      "yenny_kurniawaty@telkomsel.co.id,";
      "lega@kramatdjatigroup.com,";
      "stadiumcafe@indonet.id,";
      "agencyfreestylebali@yahoo.com,";
      "yayaqdarma@yahoo.co.id,";
      "hanafiid@yahoo.com,";
      "ricky_dvt@yahoo.co.id,";
      "teuku_umar@binus-centre.com,";
      "flp_bali@yahoo.com,";
      "andy@ritzcarlton-bali.com,";
      "bapakbakery@dps.centrin.net.id,";
      "siddiq@teacher.com,";
      "clipper@indo.net.id,";
      "puricendana@yahoo.com,";
      "info@ripcurlschoolsurf.com,";
      "sales@ramabeachhotel.com,";
      "healing@indosat.net.id,";
      "djinaldi@yahoo.co.uk,";
      "rotary.bali.kuta@gmail.com,";
      "dadang@ma-joly.com,";
      "takenoko_bali@yahoo.co.id,";
      "hrd@novotelbali.com,";
      "purwa@kcb-tours.com,";
      "anggie.gendut@england.com,";
      "novyog@indo.net.id,";
      "reservation@meliabali.com,";
      "sales@meliabali.com,";
      "info@rkeconsulting.com,";
      "andisetiaji@abacus-ind.co.id,";
      "sales.corp@swissgrandbali.com,";
      "karsana.wirajaya@trac.astra.co.id,";
      "muliatr@indosat.net.id,";
      "nita@surfer-girl.com,";
      "diah.permana@bagus-discovery.com,";
      "purwabali@yahoo.com,";
      "oly@islandconcpets.com,";
      "info@islandconcepts.com,";
      "gag@indo.net.id,";
      "gkumala@indosat.net.id,";
      "thegardeniavillas@meliabali.com,";
      "purchasing.mgr@thelegianbali.com,";
      "info@paradisebaliholidays.com,";
      "agus.winarko@bagus-discovery.com,";
      "cozytimes26@yahoo.com,";
      "info@papua-adventures.com,";
      "lokasaribali@hotmail.com,";
      "wahana@baliforyou.com,";
      "Stephen@victuslife.com,";
      "operations@atlasbalitours.com,";
      "balicoffeeshop@hotmail.com,";
      "mayakutacentre@telkom.net,";
      "rikmawan@dps.centrin.net.id,";
      "ndbt@bagus-discovery.com,";
      "info@indographs.com,";
      "aridwan_sgb@yahoo.com,";
      "bali@atmosphere.co.id,";
      "plmgrd@indosat.net.id,";
      "balibless@padmaubud.biz,";
      "baliaura@yahoo.com,";
      "andalan@bali.net,";
      "dmandiri@indo.net.id,";
      "pernadi@rad.net.id,";
      "Tabetha@BeyondMenus.com,";
      "adityafood@yahoo.com,";
      "sarana_com@yahoo.com,";
      "pasadena@chek.com,";
      "sales@pica-pica.com,";
      "menara_fbi@hotmail.com,";
      "home_treasure@hotmail.com,";
      "aamsalim@dps.centrin.net.id,";
      "shell_enoproduction@yahoo.com,";
      "geckoleather@hotmail.com,";
      "milagro_bali@hotmail.com,";
      "gemini19id@yahoo.com,";
      "karyacargo@dps.centrin.net.id,";
      "darabali@indo.net.id,";
      "padiprada@hotmail.com,";
      "vijowiz@yahoo.com,";
      "cafejimbaran@mekarsaribali.com,";
      "isnamks@yahoo.com,";
      "sales@allseasonslegian.com,";
      "chitra@cangguclub.com,";
      "cheriaM@xl.co.id,";
      "geo-trek@dps.centrin.net.id,";
      "sales@balipasadena.com,";
      "sales@villahening.com,";
      "fc@novotelbali.com,";
      "maolbing83@yahoo.co.id,";
      "info@dimensitropika.com,";
      "news@tabloidpiknik.com,";
      "mediacentre@bali-tourism.com,";
      "bioland-bali@telkom.net,";
      "glf-bali@indo.net.id,";
      "info@asiabali.com,";
      "takanit@yahoo.com,";
      "jamal@hrbc-bali.co.id,";
      "naniek@alilahotels.com,";
      "ndbtdps@dps.mega.net.id,";
      "mbcbali_jaka@yahoo.com,";
      "masnyonya@telkom.net,";
      "merrystravel@dps.centrin.net.id,";
      "mail@baliintermedia.com,";
      "mitrakridamandiri@hotmail.com,";
      "kartikaplz@denpasar.wasantara.net.id,";
      "oedps@indosat.net.id,";
      "jalirest@indosat.net.id,";
      "jenni_hartatik@interconti.com,";
      "info@alamkulkul.com,";
      "info@aggacitta.com,";
      "info@jasatours.com,";
      "iskandar.Liemena@idn.xerox.com,";
      "info@lorinresortsababai.com,";
      "ketutsukarta@telkom.net,";
      "renata.hutasoit@hyattintl.com,";
      "sukiato@hotelpadma.com,";
      "salesser@idola.net.id,";
      "sales@bali-clubaqua.com,";
      "sales@amandaresort.com,";
      "sales@balimandira.com,";
      "reservation@ramacandidasahotel.com,";
      "reservation@puriwulandari.net,";
      "nathanhotel@eksadata.com,";
      "rudi_chandra@kartikaplaza.co.id,";
      "ndcmdo@indosat.net.id,";
      "reservation@ramayanahotel.com,";
      "paradiso_bowlingbilliard@hotmail.com,";
      "perdana@balioffice.com,";
      "putribali@denpasar.wasantara.net.id,";
      "psmovers@indo.net.id,";
      "ops@thebale.com,";
      "hapsar@burung.org,";
      "ramayana@dps.mega.net.id,";
      "securanto@yahoo.com,";
      "info@villakendil.com,";
      "reservation@mpkm.co.id,";
      "info@armaresort.com,";
      "sales@balihaicruises.com,";
      "warsaubud@hotmail.com,";
      "bali_promo@plasa.com,";
      "gratindo@hotmail.com,";
      "rupadhatu89@yahoo.co.uk,";
      "info@balivillage.com,";
      "info@tomahouse.com,";
      "sales.ta@swissgrandbali.com,";
      "baliwastafel@yahoo.com,";
      "product.dps@marintur.co.id,";
      "marindps@indo.net.id,";
      "operation@cnptours.com,";
      "panoramahtl@indo.net.id,";
      "pru@indo.net.id,";
      "balivillage@indo.net.id,";
      "huzni@holidayvilla.com,";
      "info@balibmrdive.com,";
      "gadis0381@yahoo.com,";
      "info@balijazzfestival.com,";
      "triple_ebali@yahoo.com,";
      "pimage@indosat.net,";
      "kikuyaart@yahoo.com,";
      "polystar@cbn.net.id,";
      "devie@mpkm.co.id,";
      "duasisi@indo.net.id,";
      "info@palanquinbali.com,";
      "dhvbali@indosat.net.id,";
      "hussain@indo.net.id,";
      "orientalrugs_imsharif@hotmail.com,";
      "utut-irawan@ramayanahotel.com,";
      "randd98@hotmail.com,";
      "purbasari@divre7.telkom.co.id,";
      "mobnas_intim@indo.net.id,";
      "dwilasmin@yahoo.com,";
      "info@downtownbali.com,";
      "info@hotellumbung.com,";
      "info@balitonys.com,";
      "info@thevillas.net,";
      "reservation@the-dusun.com,";
      "info@theahimsa.com,";
      "info@sienna-villas.com,";
      "sababai@indosat.net.id,";
      "sales@putubalivilla.com,";
      "sales@akhyativillas.com,";
      "desamuda@indosat.net.id,";
      "reservation@amandaresort.com,";
      "info@alubali.com,";
      "vilarm@indo.net.id,";
      "intansalesbali@intanhotels.com,";
      "info@theoberoi-bali.com,";
      "legian@ghmhotels.com,";
      "reservation@rakharismavilla.com,";
      "wakagangga@wakaexperience.com,";
      "awing-awang@balivision.com,";
      "info@segaravillage.com,";
      "nsindhu@denpasar.wasantara.net.id,";
      "besakih@indosat.net.id,";
      "reservation@sanur.pphotels.com,";
      "info@santrian.com,";
      "info@sanurbeach.aerowisata.com,";
      "info@villaaya.com,";
      "balihyatt.inquiries@hyattintl.com,";
      "sales@coconuthomes.com,";
      "reservation@nirwanabaliresort.com,";
      "sales@balimeridien.com,";
      "bali@tuguhotels.com,";
      "villa_kharista@hommsindonesia.com,";
      "surgavillas@dps.centrin.net.id,";
      "gm@canggu.com,";
      "info@legianparadisohotel.com,";
      "sales@grand-balibeach.com,";
      "info@bintang-bali-hotel.com,";
      "sales.bali@saphir-hotels.com,";
      "rock@hardrockhotels.net,";
      "info@thevirabali.com,";
      "inrisata@indosat.net.id,";
      "reservation@ramabeachhotel.com,";
      "sales@jatra.com,";
      "reservation@balidynasty.com,";
      "infoadmin@mercurekutabali.com,";
      "reservation.bali@patra-jasa.com,";
      "sales@ramayanahotel.com,";
      "info@kutaparadisohotel.com,";
      "reservation@discoverykartikaplza.com,";
      "info@bluepointbayvillas.com,";
      "bcr@indosat.net.id,";
      "sales@pat-mase.com,";
      "fsrb@fourseasons.com,";
      "info@keratonjimbaranresort.com,";
      "reservation@balihai-resort.com,";
      "reservation@hotelpadma.com,";
      "adeboer@alilahotels.com,";
      "sales@kutalagoonresort.com,";
      "sales@courtyard-bali.com,";
      "balisani@indo.net.id,";
      "sales@baliholidayresort.net,";
      "sales@adhidharmahotel.com,";
      "info@whiterose.co.id,";
      "nkutabh@indosat.net.id,";
      "jhrbali@indo.net.id,";
      "rgarden@indosat.net.id,";
      "winacott@indosat.net.id,";
      "info@theoasis.info,";
      "reservation@kutaseaviewhotel.com,";
      "kbchotel@indosat.net.id,";
      "info@harris-kuta-bali.com,";
      "reservation@grandistanarama.com,";
      "sales@sahidrayabali.com,";
      "sales@pelangibali.com,";
      "info@jayakarta-lombok.com,";
      "info@hotelombak.com,";
      "hotel@novotel-lombok.com,";
      "lombokraya_htl@telkom.net,";
      "info@theoberoi-lombok.com,";
      "stay@quncivillas.com,";
      "info@poovillaclub.aerowisata.com,";
      "wakamaya@wakaexperience.com,";
      "info@senggigibeach.aerowisata.com";
      "alfabeta_ba@yahoo.com,";
      "lombok@intanhotels.com,";
      "hirlo@mataram.wasantara.net.id,";
      "sales.senggigi@sheraton.com,";
      "tulamben@mimpi.com,";
      "waterg@dps.centrin.net.id,";
      "puribaguscandidasa@bagus-dscovery.com,";
      "ramacan@denpasar,wasantara.net.id,";
      "p_saron@indo.net.id,";
      "itha@ripcurl.co.id,";
      "pwilantari@anantara.com,";
      "novie@base.co.id,";
      "manager@annorabali.com,";
      "luh_g_astitiningsih@telkomsel.co.id,";
      "kesuma.putra@kasihibuhospital.com,";
      "frisa.andarina@kasihibuhospital.com,";
      "cok.wijaya@sampoerna.com,";
      "hamartapartners@yahoo.com,";
      "mudita@indosat.com,";
      "info@phoenixgraha.com,";
      "Ni.Wiratni@sampoerna.com,";
      "budi.wiadnyana@trac.astra.co.id,";
      "Budi.Yasa@sampoerna.com,";
      "purnama.dewi@kasihibuhospital.com,";
      "david.clark5@btinternet.com";
      "manager@mibank.com";
      "reserv@nusa-lembongan.com,";
      "awinarta@anantara.com,";
      "info@sulyresort.com,";
      "sales@candibeachbali.com,";
      "sales@kamandaluresort.com,";
      "sales@ibahbali.com,";
      "bumiubud@dps.centrin.net.id,";
      "sales@barong-resort.com,";
      "info@bagusjati.com,";
      "wakapadma@wakaexperience.com,";
      "komaneka@indosat.net.id,";
      "tjampuan@indo.net.id,";
      "sales@koriubud.com,";
      "sahadewa@dps.centrin.net.id,";
      "pertiwi@indosat.net.id,";
      "kumarasakti@dps.centrin.net.id,";
      "chamsari@indosat.net,";
      "info@cahayadewatahotel.com,";
      "payogan@indosat.net.id,";
      "info@mayaubud.com,";
      "wakadiume@wakaexperience.com,";
      "balipacung@telkom.net,";
      "reservation@wantilangolfvillas.com,";
      "sales@bali-activities.com,";
      "wakanusa@wakaexperience.com,";
      "twfv@dps.centrin.net.id,";
      "menjangan@mimpi.com,";
      "wakashorea@wakaexperience.com,";
      "mbr-bali@indo.net.id,";
      "kayumanis@baliprivatevilla.com,";
      "resort@damai.com,";
      "apummer@alilahotels.com,";
      "sales@balihandarakosaido.com,";
      "bali@purisaron.com,";
      "sales@alampurivilla.com,";
      "info@villasemana.com,";
      "home@themansionbali.com,";
      "info@santimandalaresort.com,";
      "natura@indosat.net.id,";
      "puribaguslovina@bagus-discovery.com,";
      "agustiansyah@takaful.com,";
      "dewimoes@yahoo.com,";
      "denie@bigfoot.com,";
      "deden@bali-exoticwedding.com,";
      "deddydiva@yahoo.com,";
      "budivirgono@yahoo.co.id,";
      "budi@bmnlawoffice.info,";
      "ritzbc@indosat.net.id,";
      "eurobali@indosat.net.id,";
      "neginohige@hotmail.com,";
      "waow_one@yahoo.co.id,";
      "rini_wulandari1970@yahoo.co.id,";
      "priharyati@itpc.or.jp,";
      "kucing_puri@yahoo.co.jp,";
      "fadlycakp@yahoo.com,";
      "eh_juniadi@yahoo.com,";
      "balibusser@yahoo.com,";
      "bayu@bmnlawoffice.info,";
      "hanifah@bniaga.co.id,";
      "bali@indomultimedia.co.id,";
      "ary@balidestinationtravel.com,";
      "admin@sectorbarrestaurant.com,";
      "gendut@england.com,";
      "novyog@indo.net.id,anggie,";
      "amin@paradisebaliholidays.com,";
      "leebahri@yahoo.com,";
      "info@dsmbali.or.id,";
      "heni@bali-exoticwedding.com,";
      "zakat_bali@yahoo.com,";
      "tothesolo@yahoo.com,";
      "hardwoodindonesia@yahoo.com,";
      "fuay@yahoo.com,";
      "fuay@walla.com,";
      "ef_architect@yahoo.com,";
      "ennymei@telkom.net,";
      "wulandari@firststatebali.com,";
      "ihwan@pollowindonesia.com,";
      "haryo.santoso@trac.astra.co.id,";
      "andrie.yudhianto@gmail.com,";
      "fidiyono_bali@yahoo.com,";
      "fauzantan@yahoo.com,";
      "faisal_silin@yahoo.com,";
      "elkahiri@yahoo.co.id,";
      "bernitha_widinansari@yahoo.co.id,";
      "a6us_kurniawan@yahoo.co.id,";
      "itha_ersita@yahoo.com,";
      "elly@pantravel.co.id,";
      "iva@nikkobali.com,";
      "elly@intrareps.com,";
      "sbtours@indosat.net.id,";
      "wisantaradps@yahoo.com,";
      "jawi@dps.centrin.net.id,";
      "info@baliadventuretours.com,";
      "alampuri@resortgallery.com,";
      "beauty_rahma@yahoo.com,";
      "dsartika@internux.net.id,";
      "sri.hadibudi@bagus-discovery.com,";
      "freddy@bali-tourism-board.com,";
      "wiwid@ripcurl.co.id,";
      "ratna.wijayanti@aig.com,";
      "nurnirwan@yahoo.com,";
      "grandjv@indo.net.id,";
      "bukitpratama@yahoo.com,";
      "awie_kasasi@yahoo.com,";
      "gdiezzmewth@yahoo.co.id,";
      "gandjar@earthling.net,";
      "cubenenni@yahoo.com,";
      "rani@bluepointbayvillas.com,";
      "preman_surabaya3@yahoo.com,";
      "nenyjon77@yahoo.com,";
      "mf_ulfa@yahoo.com,";
      "meutyahafid@yahoo.com,";
      "mayanto@yahoo.com,";
      "marketing@balivisioncomputer.com,";
      "alwin007@yahoo.com,";
      "nathansugiarto@yahoo.com";
      "dianpuri69@yahoo.com,";
      "sales@astonbali.com,";
      "ngurah_rai56@yahoo.com";
      "herjun_jp@yahoo.co.id,";
      "zip_fmmaros@yahoo.com,";
      "icendol@yahoo.com,";
      "nediarjuliadi@yahoo.com,";
      "info@novotelbali.com,";
      "w_promoplus@yahoo.com";
      "salesrrb@indosat.net.id,";
      "phio_19@yahoo.com";
      "marinasenggigi@yahoo.co.id";
      "iik_young@yahoo.com";
      "igmastika@idp.co.id";
      "hnry_stwn@yahoo.com";
      "aulFrancisKacingenta_39@yahoo.com";
      "bit_bali@yahoo.com,";
      "gunawanpnj@yahoo.com,";
      "baligh.inquires@hyattintl.com,";
      "balihai@q-net.net.id,";
      "bliss@thebale.com,";
      "info@villasekarnusadua.com,";
      "balidesa@indosat.net.id,";
      "info@swiss-bellhotel-baliaga.com,";
      "thewestinresortbali@westin.com,";
      "sheraton.laguna@luxurycollection.com,";
      "melia.benoa@solmelia.com,";
      "sales@nikkobali.com,";
      "btbbtbfauzanbabijanuitem_jelut@yahoo.com,";
      "info@balihilton.com,";
      "balireef@balireef-resort.com,";
      "vl_bintang@denpasar.wasantara.net.id,";
      "ptanjung@indo.net.id,";
      "pbenoa@denpasar.wasantara.net.id,";
      "suites@baliroyal.com,";
      "cbmrsv@indosat.net.id,";
      "sales@nusaduahotel.com,";
      "evi@discountvoucherbooklet.com,";
      "Malelakfauzansubandi@yahoo.co.id,";
      "koko_dic@yahoo.com,";
      "yusyunikamiyani@yahoo.com,";
      "widya.Riani@hrdap.mail.a.rd.honda.co.jp,";
      "novita_vasiska@yahoo.com,";
      "ngk-jkt4@cbn.net.id,";
      "lies@wika-ngk.co.id,";
      "mulyadi_kbi@yahoo.co.id,";
      "dutaos@telkom.net,";
      "neoazuma@yahoo.co.id,";
      "ikmar@citra.co.id,";
      "t_hadi_g@cp.co.id,";
      "chandra@mailcda.com,";
      "nina@mailcda.com,";
      "rsuryamega@3selaras.com,";
      "petrgilx@yahoo.co.uk,";
      "bali@interconti.com,";
      "rputra@pardic.co.id,";
      "steven_dp@yahoo.com,";
      "bevan@bahanagv.co.id";
      "balikita1@yahoo.com";
      "aguswiguna@yahoo.co.id,";
      "eddy@mgholiday.com,";
      "widiaharika@yahoo.com,";
      "srilestari@mycondradbali.com,";
      "dsujatha@indosat.net.id,";
      "clate.m@infusionsoft.com,";
      "stevenbali@hotmail.com,";
      "info@bali-tourism-board.com,";
      "trauining@triatma-mapindo.ac.id,";
      "rosariyanti@xl.co.id,";
      "mgr_eo@yahoo.co.id,";
      "missd@bigpond.net.au,";
      "getanjali.anand@seejobs.org,";
      "gus_krisna@yahoo.com";
      "tazgirls_2512@yahoo.com,";
      "santhiarsa@yahoo.com,";
      "dmsbali@yahoo.com,";
      "andri_budiarto@multibintang.co.id,";
      "Wiboko_rinarto@telkomsel.co.id,";
      "caracraft@hotmail.com,";
      "plastic_centre_sanur@hotmail.com,";
      "sandangjaya@yahoo.com,";
      "assa_tour@yahoo.com,";
      "zefanya_production@hotmail.com,";
      "etha@dps.centrin.net.id,";
      "kewayang@dps.centrin.net.id,";
      "pahalakencana@dps.centrin.net.id,";
      "balipermata@telkom.net,";
      "ops@thebale.com";
      "sales@amandaresort.com";
      "kartikaplz@denpasar.wasantara.net.id";
      "ketutsukarta@telkom.net";
      "mail@baliintermedia.com";
      "merrystravel@dps.centrin.net.id";
      "masnyonya@telkom.net";
      "mbcbali_jaka@yahoo.com";
      "ndbtdps@dps.mega.net.id";
      "naniek@alilahotels.com";
      "nathanhotel@eksadata.com";
      "jalirest@indosat.net.id";
      "oedps@indosat.net.id";
      "jenni_hartatik@interconti.com";
      "psmovers@indo.net.id";
      "putribali@denpasar.wasantara.net.id";
      "perdana@balioffice.com";
      "paradiso_bowlingbilliard@hotmail.com";
      "reservation@ramayanahotel.com";
      "renata.hutasoit@hyattintl.com";
      "rudi_chandra@kartikaplaza.co.id";
      "ramayana@dps.mega.net.id";
      "reservation@puriwulandari.net";
      "reservation@ramacandidasahotel.com";
      "wibawa@mas-travel.com";
      "ndcmdo@indosat.net.id";
      "pimage@indosat.net";
      "mobnas_intim@indo.net.id";
      "purbasari@divre7.telkom.co.id";
      "randd98@hotmail.com";
      "utut-irawan@ramayanahotel.com";
      "orientalrugs_imsharif@hotmail.com";
      "hussain@indo.net.id";
      "dhvbali@indosat.net.id";
      "huzni@holidayvilla.com";
      "duasisi@indo.net.id";
      "dwilasmin@yahoo.com";
      "jamal@hrbc-bali.co.id";
      "kikuyaart@yahoo.com";
      "sales@bali-clubaqua.com";
      "triple_ebali@yahoo.com";
      "info@balijazzfestival.com";
      "gadis0381@yahoo.com";
      "hapsar@burung.org";
      "info@palanquinbali.com";
      "info@villakendil.com";
      "info@lorinresortsababai.com";
      "iskandar.Liemena@idn.xerox.com";
      "info@jasatours.com";
      "info@aggacitta.com";
      "info@alamkulkul.com";
      "polystar@cbn.net.id";
      "santika@santikabali.com";
      "sales@balimandira.com";
      "info@thejimbaranbali.com";
      "sales@thejimbaranbali.com";
      "tubanrestaurant@yahoo.com";
      "ib.suparsa@yahoo.com";
      "vicmgr@i-xplore.com";
      "info@risatabali.com";
      "ananda_resort@hotmail.com";
      "parigata@indosat.net.id";
      "pch@novotelbali.com";
      "Winnie@grandlingga.com";
      "BAL.Purchasing@fourseasons.com";
      "juaidy_asia@yahoo.com";
      "sariati@sanurbeach.aerowisata.com";
      "purchasing@meliabali.com";
      "sales@legianbeachbali.com";
      "bali@pansea.com";
      "vendra@keratonjimbaranresort.com";
      "phbalichef@indo.net.id";
      "yuni6671@gmail.com";
      "griyasantrian@santrian.com";
      "bounty@indo.net.id";
      "swa_candra@yahoo.com";
      "swacandra@telkom.net";
      "info@kindvillabintang.com";
      "vivi@kopibali.com";
      "salesser@idola.net.id";
      "sukiato@hotelpadma.com";
      "stadiumcafe@indonet.id";
      "trvlindo@upg.mega.net.id";
      "thegardeniavillas@meliabali.com";
      "teraoka@his-bali.com";
      "trustbali@balidream.com";
      "thebarbali@hotmail.com";
      "ustad_july@yahoo.com";
      "ubud@pansea.com";
      "sari@bubbagumpbali.com";
      "villaseminyak@eksadata.com";
      "devie@mpkm.co.id";
      "waterbom@denpasar.wasantara.net.id";
      "winarios@indosat.net.id";
      "zsa@eyeview.info";
      "moka@dps.mega.net.id";
      "matt.lloyd@roamfree.com";
      "info@balicateringcompany.com";
      "chef@nusaduahotel.com";
      "info@jenggala-bali.com";
      "gwkbali@indosat.net.id";
      "project@balihai-rsort.com";
      "peninsulabali@dps.centrin.net.id";
      "ust.july@mas-travel.com";
      "ndbt@bagus-discovery.com";
      "info@tomahouse.com";
      "info@paradisebaliholidays.com";
      "agus.winarko@bagus-discovery.com";
      "cozytimes26@yahoo.com";
      "info@papua-adventures.com";
      "lokasaribali@hotmail.com";
      "plmgrd@indosat.net.id";
      "Stephen@victuslife.com";
      "gkumala@indosat.net.id";
      "balicoffeeshop@hotmail.com";
      "dmandiri@indo.net.id";
      "rikmawan@dps.centrin.net.id";
      "pernadi@rad.net.id";
      "info@indographs.com";
      "aridwan_sgb@yahoo.com";
      "sales.corp@swissgrandbali.com";
      "operations@atlasbalitours.com";
      "wahana@baliforyou.com";
      "hrd@novotelbali.com";
      "purwa@kcb-tours.com";
      "anggie.gendut@england.com";
    ]
  in
  let th0 =
    let open Part in
    let* res = create (Fpath.to_string path) in
    match res with
    | Error (`Msg err) -> Alcotest.failf "%s." err
    | Ok () ->
        let* () = open_index writer ~path:(Fpath.to_string path) in
        let rec go0 idx = function
          | [] -> return ()
          | key :: r ->
              let* _ = insert (Rowex.key key) idx in
              go0 (succ idx) r
        in
        let* _ = go0 0 elts in
        let* _ = insert (Rowex.key "novyog@indo.net.id") (-1) in
        let* v' = find (Rowex.key "novyog@indo.net.id") in
        Alcotest.(check int) "novyog@indo.net.id" v' (-1);
        let rec go1 idx = function
          | [] -> close
          | key :: r ->
              let* v' = find (Rowex.key key) in
              Alcotest.(check int) key v' idx;
              go1 (succ idx) r
        in
        go1 0 elts
  in
  match Part.(run closed th0) with
  | _closed, () -> ()
  | exception Rowex.Duplicate ->
      Alcotest.failf "Insert a duplicate into the index"
  | exception Not_found -> Alcotest.failf "Key not found"

let test11 =
  Alcotest.test_case "test11" `Quick @@ fun path ->
  let state = state_of_optional_path path in
  let th0 =
    let open Part in
    let* _ = insert (Rowex.key "abc") 1 in
    let* _ = insert (Rowex.key "ab") 2 in
    let* _ = insert (Rowex.key "abcde") 3 in
    let* _ = remove (Rowex.key "abcde") in
    find (Rowex.key "abcde")
  in
  match Part.run state th0 with
  | _not_closed, _ -> Alcotest.failf "Unexpected good process"
  | exception Rowex.Duplicate ->
      Alcotest.failf "Insert a duplicate into the index"
  | exception Not_found -> ()

let test12 =
  Alcotest.test_case "test13" `Quick @@ fun _path ->
  let path = Rresult.R.get_ok (Bos.OS.File.tmp "index-%s") in
  let elts =
    [
      "adhidharma@denpasar.wasantara.net.id";
      "centralreservation@ramayanahotel.com";
      "apribadi@balimandira.com";
      "cdagenhart@ifc.org";
      "dana_supriyanto@interconti.com";
      "dos@novotelbali.com";
      "daniel@hotelpadma.com";
      "daniel@balibless.com";
      "djoko_p@jayakartahotelsresorts.com";
      "expdepot@indosat.net.id";
      "feby.adamsyah@idn.xerox.com";
      "christian_rizal@interconti.com";
      "singgih93@mailcity.com";
      "idonk_gebhoy@yahoo.com";
      "info@houseofbali.com";
      "kyohana@toureast.net";
      "sales@nusaduahotel.com";
      "jayakarta@mataram.wasantara.net.id";
      "mapindo@indo.net.id";
      "sm@ramayanahotel.com";
      "anekabeach@dps.centrin.net.id";
      "yogya@jayakartahotelsresorts.com";
      "garudawisatajaya@indo.net.id";
      "ketut@kbatur.com";
      "bondps@bonansatours.com";
      "witamgr@dps.centrin.net.id";
      "dtedja@indosat.net.id";
      "info@stpbali.ac.id";
      "baliprestigeho@dps.centrin.net.id";
      "pamilu@mas-travel.com";
      "amandabl@indosat.net.id";
      "marketing@csdwholiday.com";
      "luha89@yahoo.com";
      "indahsuluh2002@yahoo.com.sg";
      "imz1991@yahoo.com";
      "gus_war81@yahoo.com";
      "kf034@indosat.net.id";
      "800produkwil@posindonesia.co.id";
      "kontak.synergi@yahoo.com";
      "oekaoeka@yahoo.com";
      "fitrianti@hotmail.com";
      "meylina310@yahoo.com";
      "h4ntoro@yahoo.com";
      "novi_enbe@yahoo.com";
      "dila_dewata@yahoo.co.id";
      "tiena_asfary@yahoo.co.id";
      "da_lawoffice@yahoo.com";
      "rini@ncsecurities.biz";
      "sudarnoto_hakim@yahoo.com";
      "wastioke@yahoo.com";
      "leebahri@yahoo.com.";
      "lia_kiara97@yahoo.com";
      "rido@weddingku.com";
      "b_astuti@telkomsel.co.id";
      "garudawisata@indo.net.id";
      "grfurniture@yahoo.com";
      "gosyen2000@hotmail.com";
      "hvhfood@indosat.net.id";
      "hr@astonbali.com";
      "hary@wibisono-family.com";
      "fadlycak'p@yahoo.com";
      "ida_sampurniah@telkomsel.co.id";
      "muslim-pariwisata-bali@yahoogroups.com";
      "harisnira@yahoo.com";
      "sales@houseofbali.com";
      "baim_ron@yahoo.com";
      "ilhambali222@yahoo.com";
      "bungjon@gmail.com";
      "diar@bdg.centrin.net.id";
      "elmienruge@hotmail.com";
      "galaxygarden2006@yahoo.com";
      "gorisata@indosat.net.id";
      "maulitasarihani@yahoo.com";
      "hamiluddakwah@gmail.com.au";
      "bounty@indo.net.id,";
      "michi@ritzcarlton-bali.com,";
      "orridor@dps.centrin.net.id,";
      "ngumina@hotmail.com,";
      "made@mas-travel.com,";
      "evi@mas-travel.com,";
      "wibawa@mas-travel.com,";
      "saihubaly@yahoo.co.id,";
      "swa_candra@yahoo.com,";
      "picapica@denpasar.wasantara.net.id,";
      "griyasantrian@santrian.com,";
      "yuni6671@gmail.com,";
      "phbalichef@indo.net.id,";
      "vendra@keratonjimbaranresort.com,";
      "bali@pansea.com,";
      "sales@legianbeachbali.com,";
      "purchasing@meliabali.com,";
      "swacandra@telkom.net,";
      "lysbeth@paintballbali.com,";
      "trvlindo@upg.mega.net.id,";
      "lim_thefaith@yahoo.com,";
      "uungtb@yahoo.com.au,";
      "vivaldil307@hotmail.com,";
      "iodakon@yahoo.co.id,";
      "reservation@pendawahotel.com,";
      "ptbon@dps.centrin.net.id,";
      "ptlamak@indosat.net.id,";
      "sculpt@indo.net.id,";
      "memedi-gwkbali@dps.centrin.net.id,";
      "info@leisuredream.com,";
      "indra_wijaya@hero.co.id,";
      "ndbconvex@bagus-discovery.com,";
      "Endro@bma-merdeka.com,";
      "wsuardana@indosat.net.id,";
      "bali@esmirada.com,";
      "BAL.Purchasing@fourseasons.com,";
      "ruby@marthatilaar-spa.com,";
      "villaseminyak@eksadata.com,";
      "sariati@sanurbeach.aerowisata.com,";
      "info@jenggala-bali.com,";
      "chef@nusaduahotel.com,";
      "info@balicateringcompany.com,";
      "moka@dps.mega.net.id,";
      "zsa@eyeview.info,";
      "winarios@indosat.net.id,";
      "project@balihai-rsort.com,";
      "vivi@kopibali.com,";
      "peninsulabali@dps.centrin.net.id,";
      "ust.july@mas-travel.com,";
      "ubud@pansea.com,";
      "ustad_july@yahoo.com,";
      "thebarbali@hotmail.com,";
      "trustbali@balidream.com,";
      "teraoka@his-bali.com,";
      "candle@dps.centrin.net.id,";
      "waterbom@denpasar.wasantara.net.id,";
      "ib.suparsa@yahoo.com,";
      "budhipra@nesiancea.com,";
      "info@kindvillabintang.com,";
      "pch@novotelbali.com,";
      "parigata@indosat.net.id,";
      "mail@grandmirage.com,";
      "ananda_resort@hotmail.com,";
      "info@risatabali.com,";
      "gwkbali@indosat.net.id,";
      "rai@gosharestaurant.com,";
      "santika@santikabali.com,";
      "sahidbl@indosat.net.id,";
      "tubanrestaurant@yahoo.com,";
      "sales@thejimbaranbali.com,";
      "info@thejimbaranbali.com,";
      "sari@bubbagumpbali.com,";
      "Winnie@grandlingga.com,";
      "juaidy_asia@yahoo.com,";
      "vicmgr@i-xplore.com,";
      "langka@theclubstore.co.id,";
      "lilakresna@ConradBali.com,";
      "wayan.atmaja@luxurycollecton.com,";
      "Cisabali@indo.net.id,";
      "garrant@indo.net.id,";
      "owenwister@yahoo.com,";
      "tiara@dps.mega.net.id,";
      "info@nzmuslim.net,";
      "yuanito.kurniawan@sea.ccamatil.com,";
      "pitamaha@indosat.net.id,";
      "yunani@theclubstore.co.id,";
      "deklis@hotmail.com,";
      "cianjur@indo.net.id,";
      "mahajayatower@hotmail.com,";
      "endra@centrin.net.id,";
      "wayan.dirayana@fourseasons.com,";
      "balinaga@dps.centrin.net.id,";
      "tiaradwt@dps.centrin.net.id,";
      "candrator@hotmail.com,";
      "altaraspa@yahoo.com,";
      "fani@clubbali.com,";
      "Itudm@dps.centrin.net.id,";
      "baliratuspa@biz.net.id,";
      "kawasspa@indosat.net.id,";
      "hatoe7@yahoo.co.jp,";
      "sales@mimpi.com,";
      "theroyal@indosat.net.id,";
      "chakra_92@yahoo.com,";
      "u_dmtdps@sosro.com,";
      "januar@citramedia.net,";
      "januar@balivisioncomp.com,";
      "admin@balivisioncomp.com,";
      "ansri@dps.mega.net.id,";
      "info@rijasaresort-villas.com,";
      "sales@komaneka.com,";
      "multigun@indo.net.id,";
      "ishwari@bagus-discovery.com,";
      "utami@bali-exoticwedding.com,";
      "putra_wirata@hotmail.com,";
      "arte@dps.centrin.net.id,";
      "hamiludd2kwah@yahoo.com.au,";
      "btu_cipluk@yahoo.com,";
      "agus@indo-journey.com,";
      "agus.winarko@gmail.com,";
      "agus.amirudin@wilmar.co.id,";
      "adamsilver@lycos.com,";
      "yayasanlaroyba@yahoo.co.id,";
      "luminaABC@hotmail.com,";
      "umasapna@coconuthomes.com,";
      "udsupradinasty@yahoo.co.id,";
      "ticketing@bagus-discovery.com,";
      "tejo@pttropical.co.id,";
      "syamklw@yahoo.com,";
      "sutiarso21@yahoo.com,";
      "silvia_maniz@yahoo.com,";
      "yenny_kurniawaty@telkomsel.co.id,";
      "lega@kramatdjatigroup.com,";
      "stadiumcafe@indonet.id,";
      "agencyfreestylebali@yahoo.com,";
      "yayaqdarma@yahoo.co.id,";
      "hanafiid@yahoo.com,";
      "ricky_dvt@yahoo.co.id,";
      "teuku_umar@binus-centre.com,";
      "flp_bali@yahoo.com,";
      "andy@ritzcarlton-bali.com,";
      "bapakbakery@dps.centrin.net.id,";
      "siddiq@teacher.com,";
      "clipper@indo.net.id,";
      "puricendana@yahoo.com,";
      "info@ripcurlschoolsurf.com,";
      "sales@ramabeachhotel.com,";
      "healing@indosat.net.id,";
      "djinaldi@yahoo.co.uk,";
      "rotary.bali.kuta@gmail.com,";
      "dadang@ma-joly.com,";
      "takenoko_bali@yahoo.co.id,";
      "hrd@novotelbali.com,";
      "purwa@kcb-tours.com,";
      "anggie.gendut@england.com,";
      "novyog@indo.net.id,";
      "reservation@meliabali.com,";
      "sales@meliabali.com,";
      "info@rkeconsulting.com,";
      "andisetiaji@abacus-ind.co.id,";
      "sales.corp@swissgrandbali.com,";
      "karsana.wirajaya@trac.astra.co.id,";
      "muliatr@indosat.net.id,";
      "nita@surfer-girl.com,";
      "diah.permana@bagus-discovery.com,";
      "purwabali@yahoo.com,";
      "oly@islandconcpets.com,";
      "info@islandconcepts.com,";
      "gag@indo.net.id,";
      "gkumala@indosat.net.id,";
      "thegardeniavillas@meliabali.com,";
      "purchasing.mgr@thelegianbali.com,";
      "info@paradisebaliholidays.com,";
      "agus.winarko@bagus-discovery.com,";
      "cozytimes26@yahoo.com,";
      "info@papua-adventures.com,";
      "lokasaribali@hotmail.com,";
      "wahana@baliforyou.com,";
      "Stephen@victuslife.com,";
      "operations@atlasbalitours.com,";
      "balicoffeeshop@hotmail.com,";
      "mayakutacentre@telkom.net,";
      "rikmawan@dps.centrin.net.id,";
      "ndbt@bagus-discovery.com,";
      "info@indographs.com,";
      "aridwan_sgb@yahoo.com,";
      "bali@atmosphere.co.id,";
      "plmgrd@indosat.net.id,";
      "balibless@padmaubud.biz,";
      "baliaura@yahoo.com,";
      "andalan@bali.net,";
      "dmandiri@indo.net.id,";
      "pernadi@rad.net.id,";
      "Tabetha@BeyondMenus.com,";
      "adityafood@yahoo.com,";
      "sarana_com@yahoo.com,";
      "pasadena@chek.com,";
      "sales@pica-pica.com,";
      "menara_fbi@hotmail.com,";
      "home_treasure@hotmail.com,";
      "aamsalim@dps.centrin.net.id,";
      "shell_enoproduction@yahoo.com,";
      "geckoleather@hotmail.com,";
      "milagro_bali@hotmail.com,";
      "gemini19id@yahoo.com,";
      "karyacargo@dps.centrin.net.id,";
      "darabali@indo.net.id,";
      "padiprada@hotmail.com,";
      "vijowiz@yahoo.com,";
      "cafejimbaran@mekarsaribali.com,";
      "isnamks@yahoo.com,";
      "sales@allseasonslegian.com,";
      "chitra@cangguclub.com,";
      "cheriaM@xl.co.id,";
      "geo-trek@dps.centrin.net.id,";
      "sales@balipasadena.com,";
      "sales@villahening.com,";
      "fc@novotelbali.com,";
      "maolbing83@yahoo.co.id,";
      "info@dimensitropika.com,";
      "news@tabloidpiknik.com,";
      "mediacentre@bali-tourism.com,";
      "bioland-bali@telkom.net,";
      "glf-bali@indo.net.id,";
      "info@asiabali.com,";
      "takanit@yahoo.com,";
      "jamal@hrbc-bali.co.id,";
      "naniek@alilahotels.com,";
      "ndbtdps@dps.mega.net.id,";
      "mbcbali_jaka@yahoo.com,";
      "masnyonya@telkom.net,";
      "merrystravel@dps.centrin.net.id,";
      "mail@baliintermedia.com,";
      "mitrakridamandiri@hotmail.com,";
      "kartikaplz@denpasar.wasantara.net.id,";
      "oedps@indosat.net.id,";
      "jalirest@indosat.net.id,";
      "jenni_hartatik@interconti.com,";
      "info@alamkulkul.com,";
      "info@aggacitta.com,";
      "info@jasatours.com,";
      "iskandar.Liemena@idn.xerox.com,";
      "info@lorinresortsababai.com,";
      "ketutsukarta@telkom.net,";
      "renata.hutasoit@hyattintl.com,";
      "sukiato@hotelpadma.com,";
      "salesser@idola.net.id,";
      "sales@bali-clubaqua.com,";
      "sales@amandaresort.com,";
      "sales@balimandira.com,";
      "reservation@ramacandidasahotel.com,";
      "reservation@puriwulandari.net,";
      "nathanhotel@eksadata.com,";
      "rudi_chandra@kartikaplaza.co.id,";
      "ndcmdo@indosat.net.id,";
      "reservation@ramayanahotel.com,";
      "paradiso_bowlingbilliard@hotmail.com,";
      "perdana@balioffice.com,";
      "putribali@denpasar.wasantara.net.id,";
      "psmovers@indo.net.id,";
      "ops@thebale.com,";
      "hapsar@burung.org,";
      "ramayana@dps.mega.net.id,";
      "securanto@yahoo.com,";
      "info@villakendil.com,";
      "reservation@mpkm.co.id,";
      "info@armaresort.com,";
      "sales@balihaicruises.com,";
      "warsaubud@hotmail.com,";
      "bali_promo@plasa.com,";
      "gratindo@hotmail.com,";
      "rupadhatu89@yahoo.co.uk,";
      "info@balivillage.com,";
      "info@tomahouse.com,";
      "sales.ta@swissgrandbali.com,";
      "baliwastafel@yahoo.com,";
      "product.dps@marintur.co.id,";
      "marindps@indo.net.id,";
      "operation@cnptours.com,";
      "panoramahtl@indo.net.id,";
      "pru@indo.net.id,";
      "balivillage@indo.net.id,";
      "huzni@holidayvilla.com,";
      "info@balibmrdive.com,";
      "gadis0381@yahoo.com,";
      "info@balijazzfestival.com,";
      "triple_ebali@yahoo.com,";
      "pimage@indosat.net,";
      "kikuyaart@yahoo.com,";
      "polystar@cbn.net.id,";
      "devie@mpkm.co.id,";
      "duasisi@indo.net.id,";
      "info@palanquinbali.com,";
      "dhvbali@indosat.net.id,";
      "hussain@indo.net.id,";
      "orientalrugs_imsharif@hotmail.com,";
      "utut-irawan@ramayanahotel.com,";
      "randd98@hotmail.com,";
      "purbasari@divre7.telkom.co.id,";
      "mobnas_intim@indo.net.id,";
      "dwilasmin@yahoo.com,";
      "info@downtownbali.com,";
      "info@hotellumbung.com,";
      "info@balitonys.com,";
      "info@thevillas.net,";
      "reservation@the-dusun.com,";
      "info@theahimsa.com,";
      "info@sienna-villas.com,";
      "sababai@indosat.net.id,";
      "sales@putubalivilla.com,";
      "sales@akhyativillas.com,";
      "desamuda@indosat.net.id,";
      "reservation@amandaresort.com,";
      "info@alubali.com,";
      "vilarm@indo.net.id,";
      "intansalesbali@intanhotels.com,";
      "info@theoberoi-bali.com,";
      "legian@ghmhotels.com,";
      "reservation@rakharismavilla.com,";
      "wakagangga@wakaexperience.com,";
      "awing-awang@balivision.com,";
      "info@segaravillage.com,";
      "nsindhu@denpasar.wasantara.net.id,";
      "besakih@indosat.net.id,";
      "reservation@sanur.pphotels.com,";
      "info@santrian.com,";
      "info@sanurbeach.aerowisata.com,";
      "info@villaaya.com,";
      "balihyatt.inquiries@hyattintl.com,";
      "sales@coconuthomes.com,";
      "reservation@nirwanabaliresort.com,";
      "sales@balimeridien.com,";
      "bali@tuguhotels.com,";
      "villa_kharista@hommsindonesia.com,";
      "surgavillas@dps.centrin.net.id,";
      "gm@canggu.com,";
      "info@legianparadisohotel.com,";
      "sales@grand-balibeach.com,";
      "info@bintang-bali-hotel.com,";
      "sales.bali@saphir-hotels.com,";
      "rock@hardrockhotels.net,";
      "info@thevirabali.com,";
      "inrisata@indosat.net.id,";
      "reservation@ramabeachhotel.com,";
      "sales@jatra.com,";
      "reservation@balidynasty.com,";
      "infoadmin@mercurekutabali.com,";
      "reservation.bali@patra-jasa.com,";
      "sales@ramayanahotel.com,";
      "info@kutaparadisohotel.com,";
      "reservation@discoverykartikaplza.com,";
      "info@bluepointbayvillas.com,";
      "bcr@indosat.net.id,";
      "sales@pat-mase.com,";
      "fsrb@fourseasons.com,";
      "info@keratonjimbaranresort.com,";
      "reservation@balihai-resort.com,";
      "reservation@hotelpadma.com,";
      "adeboer@alilahotels.com,";
      "sales@kutalagoonresort.com,";
      "sales@courtyard-bali.com,";
      "balisani@indo.net.id,";
      "sales@baliholidayresort.net,";
      "sales@adhidharmahotel.com,";
      "info@whiterose.co.id,";
      "nkutabh@indosat.net.id,";
      "jhrbali@indo.net.id,";
      "rgarden@indosat.net.id,";
      "winacott@indosat.net.id,";
      "info@theoasis.info,";
      "reservation@kutaseaviewhotel.com,";
      "kbchotel@indosat.net.id,";
      "info@harris-kuta-bali.com,";
      "reservation@grandistanarama.com,";
      "sales@sahidrayabali.com,";
      "sales@pelangibali.com,";
      "info@jayakarta-lombok.com,";
      "info@hotelombak.com,";
      "hotel@novotel-lombok.com,";
      "lombokraya_htl@telkom.net,";
      "info@theoberoi-lombok.com,";
      "stay@quncivillas.com,";
      "info@poovillaclub.aerowisata.com,";
      "wakamaya@wakaexperience.com,";
      "info@senggigibeach.aerowisata.com";
      "alfabeta_ba@yahoo.com,";
      "lombok@intanhotels.com,";
      "hirlo@mataram.wasantara.net.id,";
      "sales.senggigi@sheraton.com,";
      "tulamben@mimpi.com,";
      "waterg@dps.centrin.net.id,";
      "puribaguscandidasa@bagus-dscovery.com,";
      "ramacan@denpasar,wasantara.net.id,";
      "p_saron@indo.net.id,";
      "itha@ripcurl.co.id,";
      "pwilantari@anantara.com,";
      "novie@base.co.id,";
      "manager@annorabali.com,";
      "luh_g_astitiningsih@telkomsel.co.id,";
      "kesuma.putra@kasihibuhospital.com,";
      "frisa.andarina@kasihibuhospital.com,";
      "cok.wijaya@sampoerna.com,";
      "hamartapartners@yahoo.com,";
      "mudita@indosat.com,";
      "info@phoenixgraha.com,";
      "Ni.Wiratni@sampoerna.com,";
      "budi.wiadnyana@trac.astra.co.id,";
      "Budi.Yasa@sampoerna.com,";
      "purnama.dewi@kasihibuhospital.com,";
      "david.clark5@btinternet.com";
      "manager@mibank.com";
      "reserv@nusa-lembongan.com,";
      "awinarta@anantara.com,";
      "info@sulyresort.com,";
      "sales@candibeachbali.com,";
      "sales@kamandaluresort.com,";
      "sales@ibahbali.com,";
      "bumiubud@dps.centrin.net.id,";
      "sales@barong-resort.com,";
      "info@bagusjati.com,";
      "wakapadma@wakaexperience.com,";
      "komaneka@indosat.net.id,";
      "tjampuan@indo.net.id,";
      "sales@koriubud.com,";
      "sahadewa@dps.centrin.net.id,";
      "pertiwi@indosat.net.id,";
      "kumarasakti@dps.centrin.net.id,";
      "chamsari@indosat.net,";
      "info@cahayadewatahotel.com,";
      "payogan@indosat.net.id,";
      "info@mayaubud.com,";
      "wakadiume@wakaexperience.com,";
      "balipacung@telkom.net,";
      "reservation@wantilangolfvillas.com,";
      "sales@bali-activities.com,";
      "wakanusa@wakaexperience.com,";
      "twfv@dps.centrin.net.id,";
      "menjangan@mimpi.com,";
      "wakashorea@wakaexperience.com,";
      "mbr-bali@indo.net.id,";
      "kayumanis@baliprivatevilla.com,";
      "resort@damai.com,";
      "apummer@alilahotels.com,";
      "sales@balihandarakosaido.com,";
      "bali@purisaron.com,";
      "sales@alampurivilla.com,";
      "info@villasemana.com,";
      "home@themansionbali.com,";
      "info@santimandalaresort.com,";
      "natura@indosat.net.id,";
      "puribaguslovina@bagus-discovery.com,";
      "agustiansyah@takaful.com,";
      "dewimoes@yahoo.com,";
      "denie@bigfoot.com,";
      "deden@bali-exoticwedding.com,";
      "deddydiva@yahoo.com,";
      "budivirgono@yahoo.co.id,";
      "budi@bmnlawoffice.info,";
      "ritzbc@indosat.net.id,";
      "eurobali@indosat.net.id,";
      "neginohige@hotmail.com,";
      "waow_one@yahoo.co.id,";
      "rini_wulandari1970@yahoo.co.id,";
      "priharyati@itpc.or.jp,";
      "kucing_puri@yahoo.co.jp,";
      "fadlycakp@yahoo.com,";
      "eh_juniadi@yahoo.com,";
      "balibusser@yahoo.com,";
      "bayu@bmnlawoffice.info,";
      "hanifah@bniaga.co.id,";
      "bali@indomultimedia.co.id,";
      "ary@balidestinationtravel.com,";
      "admin@sectorbarrestaurant.com,";
      "gendut@england.com,";
      "novyog@indo.net.id,anggie,";
      "amin@paradisebaliholidays.com,";
      "leebahri@yahoo.com,";
      "info@dsmbali.or.id,";
      "heni@bali-exoticwedding.com,";
      "zakat_bali@yahoo.com,";
      "tothesolo@yahoo.com,";
      "hardwoodindonesia@yahoo.com,";
      "fuay@yahoo.com,";
      "fuay@walla.com,";
      "ef_architect@yahoo.com,";
      "ennymei@telkom.net,";
      "wulandari@firststatebali.com,";
      "ihwan@pollowindonesia.com,";
      "haryo.santoso@trac.astra.co.id,";
      "andrie.yudhianto@gmail.com,";
      "fidiyono_bali@yahoo.com,";
      "fauzantan@yahoo.com,";
      "faisal_silin@yahoo.com,";
      "elkahiri@yahoo.co.id,";
      "bernitha_widinansari@yahoo.co.id,";
      "a6us_kurniawan@yahoo.co.id,";
      "itha_ersita@yahoo.com,";
      "elly@pantravel.co.id,";
      "iva@nikkobali.com,";
      "elly@intrareps.com,";
      "sbtours@indosat.net.id,";
      "wisantaradps@yahoo.com,";
      "jawi@dps.centrin.net.id,";
      "info@baliadventuretours.com,";
      "alampuri@resortgallery.com,";
      "beauty_rahma@yahoo.com,";
      "dsartika@internux.net.id,";
      "sri.hadibudi@bagus-discovery.com,";
      "freddy@bali-tourism-board.com,";
      "wiwid@ripcurl.co.id,";
      "ratna.wijayanti@aig.com,";
      "nurnirwan@yahoo.com,";
      "grandjv@indo.net.id,";
      "bukitpratama@yahoo.com,";
      "awie_kasasi@yahoo.com,";
      "gdiezzmewth@yahoo.co.id,";
      "gandjar@earthling.net,";
      "cubenenni@yahoo.com,";
      "rani@bluepointbayvillas.com,";
      "preman_surabaya3@yahoo.com,";
      "nenyjon77@yahoo.com,";
      "mf_ulfa@yahoo.com,";
      "meutyahafid@yahoo.com,";
      "mayanto@yahoo.com,";
      "marketing@balivisioncomputer.com,";
      "alwin007@yahoo.com,";
      "nathansugiarto@yahoo.com";
      "dianpuri69@yahoo.com,";
      "sales@astonbali.com,";
      "ngurah_rai56@yahoo.com";
      "herjun_jp@yahoo.co.id,";
      "zip_fmmaros@yahoo.com,";
      "icendol@yahoo.com,";
      "nediarjuliadi@yahoo.com,";
      "info@novotelbali.com,";
      "w_promoplus@yahoo.com";
      "salesrrb@indosat.net.id,";
      "phio_19@yahoo.com";
      "marinasenggigi@yahoo.co.id";
      "iik_young@yahoo.com";
      "igmastika@idp.co.id";
      "hnry_stwn@yahoo.com";
      "aulFrancisKacingenta_39@yahoo.com";
      "bit_bali@yahoo.com,";
      "gunawanpnj@yahoo.com,";
      "baligh.inquires@hyattintl.com,";
      "balihai@q-net.net.id,";
      "bliss@thebale.com,";
      "info@villasekarnusadua.com,";
      "balidesa@indosat.net.id,";
      "info@swiss-bellhotel-baliaga.com,";
      "thewestinresortbali@westin.com,";
      "sheraton.laguna@luxurycollection.com,";
      "melia.benoa@solmelia.com,";
      "sales@nikkobali.com,";
      "btbbtbfauzanbabijanuitem_jelut@yahoo.com,";
      "info@balihilton.com,";
      "balireef@balireef-resort.com,";
      "vl_bintang@denpasar.wasantara.net.id,";
      "ptanjung@indo.net.id,";
      "pbenoa@denpasar.wasantara.net.id,";
      "suites@baliroyal.com,";
      "cbmrsv@indosat.net.id,";
      "sales@nusaduahotel.com,";
      "evi@discountvoucherbooklet.com,";
      "Malelakfauzansubandi@yahoo.co.id,";
      "koko_dic@yahoo.com,";
      "yusyunikamiyani@yahoo.com,";
      "widya.Riani@hrdap.mail.a.rd.honda.co.jp,";
      "novita_vasiska@yahoo.com,";
      "ngk-jkt4@cbn.net.id,";
      "lies@wika-ngk.co.id,";
      "mulyadi_kbi@yahoo.co.id,";
      "dutaos@telkom.net,";
      "neoazuma@yahoo.co.id,";
      "ikmar@citra.co.id,";
      "t_hadi_g@cp.co.id,";
      "chandra@mailcda.com,";
      "nina@mailcda.com,";
      "rsuryamega@3selaras.com,";
      "petrgilx@yahoo.co.uk,";
      "bali@interconti.com,";
      "rputra@pardic.co.id,";
      "steven_dp@yahoo.com,";
      "bevan@bahanagv.co.id";
      "balikita1@yahoo.com";
      "aguswiguna@yahoo.co.id,";
      "eddy@mgholiday.com,";
      "widiaharika@yahoo.com,";
      "srilestari@mycondradbali.com,";
      "dsujatha@indosat.net.id,";
      "clate.m@infusionsoft.com,";
      "stevenbali@hotmail.com,";
      "info@bali-tourism-board.com,";
      "trauining@triatma-mapindo.ac.id,";
      "rosariyanti@xl.co.id,";
      "mgr_eo@yahoo.co.id,";
      "missd@bigpond.net.au,";
      "getanjali.anand@seejobs.org,";
      "gus_krisna@yahoo.com";
      "tazgirls_2512@yahoo.com,";
      "santhiarsa@yahoo.com,";
      "dmsbali@yahoo.com,";
      "andri_budiarto@multibintang.co.id,";
      "Wiboko_rinarto@telkomsel.co.id,";
      "caracraft@hotmail.com,";
      "plastic_centre_sanur@hotmail.com,";
      "sandangjaya@yahoo.com,";
      "assa_tour@yahoo.com,";
      "zefanya_production@hotmail.com,";
      "etha@dps.centrin.net.id,";
      "kewayang@dps.centrin.net.id,";
      "pahalakencana@dps.centrin.net.id,";
      "balipermata@telkom.net,";
      "ops@thebale.com";
      "sales@amandaresort.com";
      "kartikaplz@denpasar.wasantara.net.id";
      "ketutsukarta@telkom.net";
      "mail@baliintermedia.com";
      "merrystravel@dps.centrin.net.id";
      "masnyonya@telkom.net";
      "mbcbali_jaka@yahoo.com";
      "ndbtdps@dps.mega.net.id";
      "naniek@alilahotels.com";
      "nathanhotel@eksadata.com";
      "jalirest@indosat.net.id";
      "oedps@indosat.net.id";
      "jenni_hartatik@interconti.com";
      "psmovers@indo.net.id";
      "putribali@denpasar.wasantara.net.id";
      "perdana@balioffice.com";
      "paradiso_bowlingbilliard@hotmail.com";
      "reservation@ramayanahotel.com";
      "renata.hutasoit@hyattintl.com";
      "rudi_chandra@kartikaplaza.co.id";
      "ramayana@dps.mega.net.id";
      "reservation@puriwulandari.net";
      "reservation@ramacandidasahotel.com";
      "wibawa@mas-travel.com";
      "ndcmdo@indosat.net.id";
      "pimage@indosat.net";
      "mobnas_intim@indo.net.id";
      "purbasari@divre7.telkom.co.id";
      "randd98@hotmail.com";
      "utut-irawan@ramayanahotel.com";
      "orientalrugs_imsharif@hotmail.com";
      "hussain@indo.net.id";
      "dhvbali@indosat.net.id";
      "huzni@holidayvilla.com";
      "duasisi@indo.net.id";
      "dwilasmin@yahoo.com";
      "jamal@hrbc-bali.co.id";
      "kikuyaart@yahoo.com";
      "sales@bali-clubaqua.com";
      "triple_ebali@yahoo.com";
      "info@balijazzfestival.com";
      "gadis0381@yahoo.com";
      "hapsar@burung.org";
      "info@palanquinbali.com";
      "info@villakendil.com";
      "info@lorinresortsababai.com";
      "iskandar.Liemena@idn.xerox.com";
      "info@jasatours.com";
      "info@aggacitta.com";
      "info@alamkulkul.com";
      "polystar@cbn.net.id";
      "santika@santikabali.com";
      "sales@balimandira.com";
      "info@thejimbaranbali.com";
      "sales@thejimbaranbali.com";
      "tubanrestaurant@yahoo.com";
      "ib.suparsa@yahoo.com";
      "vicmgr@i-xplore.com";
      "info@risatabali.com";
      "ananda_resort@hotmail.com";
      "parigata@indosat.net.id";
      "pch@novotelbali.com";
      "Winnie@grandlingga.com";
      "BAL.Purchasing@fourseasons.com";
      "juaidy_asia@yahoo.com";
      "sariati@sanurbeach.aerowisata.com";
      "purchasing@meliabali.com";
      "sales@legianbeachbali.com";
      "bali@pansea.com";
      "vendra@keratonjimbaranresort.com";
      "phbalichef@indo.net.id";
      "yuni6671@gmail.com";
      "griyasantrian@santrian.com";
      "bounty@indo.net.id";
      "swa_candra@yahoo.com";
      "swacandra@telkom.net";
      "info@kindvillabintang.com";
      "vivi@kopibali.com";
      "salesser@idola.net.id";
      "sukiato@hotelpadma.com";
      "stadiumcafe@indonet.id";
      "trvlindo@upg.mega.net.id";
      "thegardeniavillas@meliabali.com";
      "teraoka@his-bali.com";
      "trustbali@balidream.com";
      "thebarbali@hotmail.com";
      "ustad_july@yahoo.com";
      "ubud@pansea.com";
      "sari@bubbagumpbali.com";
      "villaseminyak@eksadata.com";
      "devie@mpkm.co.id";
      "waterbom@denpasar.wasantara.net.id";
      "winarios@indosat.net.id";
      "zsa@eyeview.info";
      "moka@dps.mega.net.id";
      "matt.lloyd@roamfree.com";
      "info@balicateringcompany.com";
      "chef@nusaduahotel.com";
      "info@jenggala-bali.com";
      "gwkbali@indosat.net.id";
      "project@balihai-rsort.com";
      "peninsulabali@dps.centrin.net.id";
      "ust.july@mas-travel.com";
      "ndbt@bagus-discovery.com";
      "info@tomahouse.com";
      "info@paradisebaliholidays.com";
      "agus.winarko@bagus-discovery.com";
      "cozytimes26@yahoo.com";
      "info@papua-adventures.com";
      "lokasaribali@hotmail.com";
      "plmgrd@indosat.net.id";
      "Stephen@victuslife.com";
      "gkumala@indosat.net.id";
      "balicoffeeshop@hotmail.com";
      "dmandiri@indo.net.id";
      "rikmawan@dps.centrin.net.id";
      "pernadi@rad.net.id";
      "info@indographs.com";
      "aridwan_sgb@yahoo.com";
      "sales.corp@swissgrandbali.com";
      "operations@atlasbalitours.com";
      "wahana@baliforyou.com";
      "hrd@novotelbali.com";
      "purwa@kcb-tours.com";
      "anggie.gendut@england.com";
    ]
  in
  let th0 =
    let open Part in
    let* res = create (Fpath.to_string path) in
    match res with
    | Error (`Msg err) -> Alcotest.failf "%s." err
    | Ok () ->
        let* () = open_index writer ~path:(Fpath.to_string path) in
        let rec go0 idx = function
          | [] -> return ()
          | key :: r ->
              let* _ = insert (Rowex.key key) idx in
              go0 (succ idx) r
        in
        let* _ = go0 0 elts in
        let rec consistent idx = function
          | [] -> return true
          | key :: r ->
              let* v' = find (Rowex.key key) in
              Logs.debug (fun m -> m "%S => %d (expected: %d)" key v' idx);
              if v' = idx then consistent (succ idx) r else return false
        in
        let rec go1 idx = function
          | [] -> return `Ok
          | key :: r ->
              Logs.debug (fun m -> m "Remove %S" key);
              let* () = remove (Rowex.key key) in
              let* consistent = consistent (succ idx) r in
              Logs.debug (fun m -> m "Consistent? %b" consistent);
              let* exists = exists (Rowex.key key) in
              Logs.debug (fun m -> m "%S exists? %b" key exists);
              if not consistent then return `Inconsistent
              else if exists then return (`Exists key)
              else go1 (succ idx) r
        in
        go1 0 elts
  in
  match Part.(run closed th0) with
  | _closed, `Ok -> ()
  | _closed, `Exists key -> Alcotest.failf "%S exists" key
  | _closed, `Inconsistent -> Alcotest.failf "The tree is inconsistent"
  | exception Rowex.Duplicate ->
      Alcotest.failf "Insert a duplicate into the index"
  | exception Not_found -> Alcotest.failf "Unexpected Not_found exception"

open Cmdliner

let filename =
  let parser x =
    match Fpath.of_string x with
    | Ok v when not (Sys.file_exists x) -> (
        match Part.(run closed (create (Fpath.to_string v))) with
        | _closed, Ok () -> Ok (Fpath.to_string v)
        | _closed, Error err -> Error err)
    | Ok v -> Rresult.R.error_msgf "%a already exists" Fpath.pp v
    | Error _ as err -> err
  in
  let pp ppf _ = Fmt.pf ppf "#index" in
  Arg.conv (parser, pp)

let filename =
  let doc = "The persistent index file." in
  Arg.(value & opt (some filename) None & info [ "index" ] ~doc)

let () =
  Alcotest.run_with_args "rowex" filename
    [
      ( "simple",
        [
          test01;
          test02;
          test03;
          test04;
          test05;
          test06;
          test07;
          test08;
          test09;
          test10;
          test11;
          test12;
        ] );
    ]
