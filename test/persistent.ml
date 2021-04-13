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

let size_of_word = Sys.word_size / 8

open Persistent

let identity x = x
let empty = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0

let create filename = Part.create filename

let page_size = 4096

let mmu_of_file filename = Part.unsafe_mmu_of_file filename

let random_index =
  Lazy.from_fun @@ fun () ->
  let open Rresult in
  Bos.OS.File.tmp "index-%s" >>| fun index ->
  create (Fpath.to_string index) ; mmu_of_file (Fpath.to_string index)

let mmu_of_optional_file = function
  | Some mmu -> mmu
  | None -> Rresult.R.failwith_error_msg (Lazy.force random_index)

let insert mmu root key v = run mmu (Persistent.insert root (Rowex.key key) v)
let find mmu root key = run mmu (Persistent.find root (Rowex.key key))

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a][%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Blue (fmt "%10d")) (Unix.getpid ())
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let setup_logs style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (reporter Fmt.stderr)

let () = setup_logs (Some `Ansi_tty) (Some Logs.Debug)

let test01 =
  Alcotest.test_case "test01" `Quick @@ fun file ->
  let mmu = mmu_of_optional_file file in
  Part.insert mmu "abc"   1 ;
  Alcotest.(check int) "abc"   (Part.lookup mmu "abc")   1 ;
  Part.insert mmu "ab"    2 ;
  Alcotest.(check int) "abc"   (Part.lookup mmu "abc")   1 ;
  Alcotest.(check int) "ab"    (Part.lookup mmu "ab")    2 ;
  Part.insert mmu "abcde" 3 ;
  Alcotest.(check int) "abc"   (Part.lookup mmu "abc")   1 ;
  Alcotest.(check int) "ab"    (Part.lookup mmu "ab")    2 ;
  Alcotest.(check int) "abcde" (Part.lookup mmu "abcde") 3
;;

let test02 =
  Alcotest.test_case "test02" `Quick @@ fun file ->
  let mmu = mmu_of_optional_file file in
  Part.insert mmu "a0" 0 ;
  Part.insert mmu "a1" 1 ;
  Part.insert mmu "a2" 2 ;
  Part.insert mmu "a3" 3 ;
  Alcotest.(check int) "a0" (Part.lookup mmu "a0") 0 ;
  Alcotest.(check int) "a1" (Part.lookup mmu "a1") 1 ;
  Alcotest.(check int) "a2" (Part.lookup mmu "a2") 2 ;
  Alcotest.(check int) "a3" (Part.lookup mmu "a3") 3 ;
  Part.insert mmu "a4" 4 ;
  Alcotest.(check int) "a0" (Part.lookup mmu "a0") 0 ;
  Alcotest.(check int) "a1" (Part.lookup mmu "a1") 1 ;
  Alcotest.(check int) "a2" (Part.lookup mmu "a2") 2 ;
  Alcotest.(check int) "a3" (Part.lookup mmu "a3") 3 ;
  Alcotest.(check int) "a4" (Part.lookup mmu "a4") 4
;;

let random_string len =
  let res = Bytes.create len in
  for i = 0 to len - 1 do Bytes.set res i (Char.chr (1 + Random.int 255)) done ;
  Bytes.unsafe_to_string res

let test03 =
  Alcotest.test_case "test03" `Quick @@ fun file ->
  let max = 500 in
  let mmu = mmu_of_optional_file file in
  let vs = List.init max (fun _ -> random_string (1 + Random.int 63), Random.int max) in
  List.iter (fun (k, v) -> Part.insert mmu k v) vs ;
  Alcotest.(check pass) "insertion" () () ;
  let check k v =
    let v' = Part.lookup mmu k in
    Alcotest.(check int) (Fmt.str "%S" k) v' v in
  List.iter (fun (k, v) -> check k v) vs
;;

let test04 =
  Alcotest.test_case "test04" `Quick @@ fun file ->
  let mmu = mmu_of_optional_file file in
  Part.insert mmu "stone@meekness.com" 0 ;
  Part.insert mmu "ca-tech@dps,centrin.net.id" 1 ;
  Part.insert mmu "trinanda_lestyowati@elkomsel.co.id" 2 ;
  Part.insert mmu "asst_dos@asonrasuna.com" 3 ;
  Part.insert mmu "amartabali@dps.centrim.net.id" 4 ;
  Part.insert mmu "achatv@cbn.net.id" 5 ;
  Part.insert mmu "bali@tuguhotels.com" 6 ;
  Part.insert mmu "baliminimalist@yahoo.com" 7 ; (* prefix with [li] on a n16 node *)
  Alcotest.(check int) "bali@tuguhotels.com" (Part.lookup mmu "bali@tuguhotels.com") 6 ;
  Alcotest.(check int) "baliminimalist@yahoo.com" (Part.lookup mmu "baliminimalist@yahoo.com") 7 ;
;;

let test05 =
  Alcotest.test_case "test05" `Quick @@ fun file ->
  let mmu = mmu_of_optional_file file in
  Part.insert mmu "bliss@thebale.com" 8 ;
  Alcotest.(check int) "bali@tuguhotels.com" (Part.lookup mmu "bali@tuguhotels.com") 6 ;
  Alcotest.(check int) "baliminimalist@yahoo.com" (Part.lookup mmu "baliminimalist@yahoo.com") 7 ;
  Alcotest.(check int) "bliss@thebale.com" (Part.lookup mmu "bliss@thebale.com") 8 ;
;;

let test06 =
  Alcotest.test_case "test06" `Quick @@ fun file ->
  let mmu = mmu_of_optional_file file in
  let elts =
    [ "adhidharma@denpasar.wasantara.net.id"
    ; "centralreservation@ramayanahotel.com"
    ; "apribadi@balimandira.com"
    ; "cdagenhart@ifc.org"
    ; "dana_supriyanto@interconti.com"
    ; "dos@novotelbali.com"
    ; "daniel@hotelpadma.com"
    ; "daniel@balibless.com"
    ; "djoko_p@jayakartahotelsresorts.com"
    ; "expdepot@indosat.net.id"
    ; "feby.adamsyah@idn.xerox.com"
    ; "christian_rizal@interconti.com"
    ; "singgih93@mailcity.com"
    ; "idonk_gebhoy@yahoo.com"
    ; "info@houseofbali.com"
    ; "kyohana@toureast.net"
    ; "sales@nusaduahotel.com"
    ; "jayakarta@mataram.wasantara.net.id"
    ; "mapindo@indo.net.id"
    ; "sm@ramayanahotel.com"
    ; "anekabeach@dps.centrin.net.id"
    ; "yogya@jayakartahotelsresorts.com"
    ; "garudawisatajaya@indo.net.id"
    ; "ketut@kbatur.com"
    ; "bondps@bonansatours.com"
    ; "witamgr@dps.centrin.net.id"
    ; "dtedja@indosat.net.id"
    ; "info@stpbali.ac.id"
    ; "baliprestigeho@dps.centrin.net.id"
    ; "pamilu@mas-travel.com"
    ; "amandabl@indosat.net.id"
    ; "marketing@csdwholiday.com"
    ; "luha89@yahoo.com"
    ; "indahsuluh2002@yahoo.com.sg"
    ; "imz1991@yahoo.com"
    ; "gus_war81@yahoo.com"
    ; "kf034@indosat.net.id"
    ; "800produkwil@posindonesia.co.id"
    ; "kontak.synergi@yahoo.com"
    ; "oekaoeka@yahoo.com"
    ; "fitrianti@hotmail.com"
    ; "meylina310@yahoo.com"
    ; "h4ntoro@yahoo.com"
    ; "novi_enbe@yahoo.com"
    ; "dila_dewata@yahoo.co.id"
    ; "tiena_asfary@yahoo.co.id"
    ; "da_lawoffice@yahoo.com"
    ; "rini@ncsecurities.biz"
    ; "sudarnoto_hakim@yahoo.com"
    ; "wastioke@yahoo.com"
    ; "leebahri@yahoo.com."
    ; "lia_kiara97@yahoo.com"
    ; "rido@weddingku.com"
    ; "b_astuti@telkomsel.co.id" ] in
  List.iteri (fun v k -> Part.insert mmu k v) elts ;
  let reporter = Logs.reporter () in
  Logs.set_reporter Logs.nop_reporter ;
  Fmt.epr "%a\n%!" Part.pp mmu ;
  Logs.set_reporter reporter ;
  Part.insert mmu "garudawisata@indo.net.id" (-1) ;
  let reporter = Logs.reporter () in
  Logs.set_reporter Logs.nop_reporter ;
  Fmt.epr "%a\n%!" Part.pp mmu ;
  Logs.set_reporter reporter ;
  Alcotest.(check int) "garudawisatajaya@indo.net.id" (Part.lookup mmu "garudawisatajaya@indo.net.id") 22 ;
  Alcotest.(check int) "garudawisata@indo.net.id" (Part.lookup mmu "garudawisata@indo.net.id") (-1) ;
  List.iteri (fun v' k -> Alcotest.(check int) k (Part.lookup mmu k) v') elts ;
;;

let test07 =
  Alcotest.test_case "test07" `Quick @@ fun _file ->
  let index = Rresult.R.get_ok (Bos.OS.File.tmp "index-%s") in
  create (Fpath.to_string index) ;
  let mmu = mmu_of_file (Fpath.to_string index) in
  let elts =
    [ "adhidharma@denpasar.wasantara.net.id"
    ; "centralreservation@ramayanahotel.com"
    ; "apribadi@balimandira.com"
    ; "cdagenhart@ifc.org"
    ; "dana_supriyanto@interconti.com"
    ; "dos@novotelbali.com"
    ; "daniel@hotelpadma.com"
    ; "daniel@balibless.com"
    ; "djoko_p@jayakartahotelsresorts.com"
    ; "expdepot@indosat.net.id"
    ; "feby.adamsyah@idn.xerox.com"
    ; "christian_rizal@interconti.com"
    ; "singgih93@mailcity.com"
    ; "idonk_gebhoy@yahoo.com"
    ; "info@houseofbali.com"
    ; "kyohana@toureast.net"
    ; "sales@nusaduahotel.com"
    ; "jayakarta@mataram.wasantara.net.id"
    ; "mapindo@indo.net.id"
    ; "sm@ramayanahotel.com"
    ; "anekabeach@dps.centrin.net.id"
    ; "yogya@jayakartahotelsresorts.com"
    ; "garudawisatajaya@indo.net.id"
    ; "ketut@kbatur.com"
    ; "bondps@bonansatours.com"
    ; "witamgr@dps.centrin.net.id"
    ; "dtedja@indosat.net.id"
    ; "info@stpbali.ac.id"
    ; "baliprestigeho@dps.centrin.net.id"
    ; "pamilu@mas-travel.com"
    ; "amandabl@indosat.net.id"
    ; "marketing@csdwholiday.com"
    ; "luha89@yahoo.com"
    ; "indahsuluh2002@yahoo.com.sg"
    ; "imz1991@yahoo.com"
    ; "gus_war81@yahoo.com"
    ; "kf034@indosat.net.id"
    ; "800produkwil@posindonesia.co.id"
    ; "kontak.synergi@yahoo.com"
    ; "oekaoeka@yahoo.com"
    ; "fitrianti@hotmail.com"
    ; "meylina310@yahoo.com"
    ; "h4ntoro@yahoo.com"
    ; "novi_enbe@yahoo.com"
    ; "dila_dewata@yahoo.co.id"
    ; "tiena_asfary@yahoo.co.id"
    ; "da_lawoffice@yahoo.com"
    ; "rini@ncsecurities.biz"
    ; "sudarnoto_hakim@yahoo.com"
    ; "wastioke@yahoo.com"
    ; "leebahri@yahoo.com."
    ; "lia_kiara97@yahoo.com"
    ; "rido@weddingku.com"
    ; "b_astuti@telkomsel.co.id"
    ; "garudawisata@indo.net.id"
    ; "grfurniture@yahoo.com"
    ; "gosyen2000@hotmail.com"
    ; "hvhfood@indosat.net.id"
    ; "hr@astonbali.com"
    ; "hary@wibisono-family.com"
    ; "fadlycak'p@yahoo.com"
    ; "ida_sampurniah@telkomsel.co.id"
    ; "muslim-pariwisata-bali@yahoogroups.com"
    ; "harisnira@yahoo.com"
    ; "sales@houseofbali.com"
    ; "baim_ron@yahoo.com"
    ; "ilhambali222@yahoo.com"
    ; "bungjon@gmail.com"
    ; "diar@bdg.centrin.net.id"
    ; "elmienruge@hotmail.com" ] in
  List.iteri (fun v k -> Part.insert mmu k v) elts ;
  let reporter = Logs.reporter () in
  Logs.set_reporter Logs.nop_reporter ;
  Fmt.epr "%a\n%!" Part.pp mmu ;
  Logs.set_reporter reporter ;
  Part.insert mmu "galaxygarden2006@yahoo.com" (-1) ;
  let reporter = Logs.reporter () in
  Logs.set_reporter Logs.nop_reporter ;
  Fmt.epr "%a\n%!" Part.pp mmu ;
  Logs.set_reporter reporter ;
  List.iteri (fun v' k -> Alcotest.(check int) k (Part.lookup mmu k) v') elts ;
;;

let test08 =
  Alcotest.test_case "test08" `Quick @@ fun _file ->
  let index = Rresult.R.get_ok (Bos.OS.File.tmp "index-%s") in
  create (Fpath.to_string index) ;
  let mmu = mmu_of_file (Fpath.to_string index) in
  let elts =
    [ "adhidharma@denpasar.wasantara.net.id"
    ; "centralreservation@ramayanahotel.com"
    ; "apribadi@balimandira.com"
    ; "cdagenhart@ifc.org"
    ; "dana_supriyanto@interconti.com"
    ; "dos@novotelbali.com"
    ; "daniel@hotelpadma.com"
    ; "daniel@balibless.com"
    ; "djoko_p@jayakartahotelsresorts.com"
    ; "expdepot@indosat.net.id"
    ; "feby.adamsyah@idn.xerox.com"
    ; "christian_rizal@interconti.com"
    ; "singgih93@mailcity.com"
    ; "idonk_gebhoy@yahoo.com"
    ; "info@houseofbali.com"
    ; "kyohana@toureast.net"
    ; "sales@nusaduahotel.com"
    ; "jayakarta@mataram.wasantara.net.id"
    ; "mapindo@indo.net.id"
    ; "sm@ramayanahotel.com"
    ; "anekabeach@dps.centrin.net.id"
    ; "yogya@jayakartahotelsresorts.com"
    ; "garudawisatajaya@indo.net.id"
    ; "ketut@kbatur.com"
    ; "bondps@bonansatours.com"
    ; "witamgr@dps.centrin.net.id"
    ; "dtedja@indosat.net.id"
    ; "info@stpbali.ac.id"
    ; "baliprestigeho@dps.centrin.net.id"
    ; "pamilu@mas-travel.com"
    ; "amandabl@indosat.net.id"
    ; "marketing@csdwholiday.com"
    ; "luha89@yahoo.com"
    ; "indahsuluh2002@yahoo.com.sg"
    ; "imz1991@yahoo.com"
    ; "gus_war81@yahoo.com"
    ; "kf034@indosat.net.id"
    ; "800produkwil@posindonesia.co.id"
    ; "kontak.synergi@yahoo.com"
    ; "oekaoeka@yahoo.com"
    ; "fitrianti@hotmail.com"
    ; "meylina310@yahoo.com"
    ; "h4ntoro@yahoo.com"
    ; "novi_enbe@yahoo.com"
    ; "dila_dewata@yahoo.co.id"
    ; "tiena_asfary@yahoo.co.id"
    ; "da_lawoffice@yahoo.com"
    ; "rini@ncsecurities.biz"
    ; "sudarnoto_hakim@yahoo.com"
    ; "wastioke@yahoo.com"
    ; "leebahri@yahoo.com."
    ; "lia_kiara97@yahoo.com"
    ; "rido@weddingku.com"
    ; "b_astuti@telkomsel.co.id"
    ; "garudawisata@indo.net.id"
    ; "grfurniture@yahoo.com"
    ; "gosyen2000@hotmail.com"
    ; "hvhfood@indosat.net.id"
    ; "hr@astonbali.com"
    ; "hary@wibisono-family.com"
    ; "fadlycak'p@yahoo.com"
    ; "ida_sampurniah@telkomsel.co.id"
    ; "muslim-pariwisata-bali@yahoogroups.com"
    ; "harisnira@yahoo.com"
    ; "sales@houseofbali.com"
    ; "baim_ron@yahoo.com"
    ; "ilhambali222@yahoo.com"
    ; "bungjon@gmail.com"
    ; "diar@bdg.centrin.net.id"
    ; "elmienruge@hotmail.com"
    ; "galaxygarden2006@yahoo.com"
    ; "gorisata@indosat.net.id"
    ; "maulitasarihani@yahoo.com"
    ; "hamiluddakwah@gmail.com.au"
    ; "bounty@indo.net.id,"
    ; "michi@ritzcarlton-bali.com,"
    ; "orridor@dps.centrin.net.id,"
    ; "ngumina@hotmail.com,"
    ; "made@mas-travel.com,"
    ; "evi@mas-travel.com,"
    ; "wibawa@mas-travel.com,"
    ; "saihubaly@yahoo.co.id,"
    ; "swa_candra@yahoo.com,"
    ; "picapica@denpasar.wasantara.net.id,"
    ; "griyasantrian@santrian.com,"
    ; "yuni6671@gmail.com,"
    ; "phbalichef@indo.net.id,"
    ; "vendra@keratonjimbaranresort.com,"
    ; "bali@pansea.com,"
    ; "sales@legianbeachbali.com,"
    ; "purchasing@meliabali.com,"
    ; "swacandra@telkom.net,"
    ; "lysbeth@paintballbali.com,"
    ; "trvlindo@upg.mega.net.id,"
    ; "lim_thefaith@yahoo.com,"
    ; "uungtb@yahoo.com.au,"
    ; "vivaldil307@hotmail.com,"
    ; "iodakon@yahoo.co.id,"
    ; "reservation@pendawahotel.com,"
    ; "ptbon@dps.centrin.net.id,"
    ; "ptlamak@indosat.net.id,"
    ; "sculpt@indo.net.id,"
    ; "memedi-gwkbali@dps.centrin.net.id,"
    ; "info@leisuredream.com,"
    ; "indra_wijaya@hero.co.id,"
    ; "ndbconvex@bagus-discovery.com,"
    ; "Endro@bma-merdeka.com,"
    ; "wsuardana@indosat.net.id,"
    ; "bali@esmirada.com,"
    ; "BAL.Purchasing@fourseasons.com,"
    ; "ruby@marthatilaar-spa.com,"
    ; "villaseminyak@eksadata.com,"
    ; "sariati@sanurbeach.aerowisata.com,"
    ; "info@jenggala-bali.com,"
    ; "chef@nusaduahotel.com,"
    ; "info@balicateringcompany.com,"
    ; "moka@dps.mega.net.id,"
    ; "zsa@eyeview.info,"
    ; "winarios@indosat.net.id,"
    ; "project@balihai-rsort.com,"
    ; "vivi@kopibali.com,"
    ; "peninsulabali@dps.centrin.net.id,"
    ; "ust.july@mas-travel.com,"
    ; "ubud@pansea.com,"
    ; "ustad_july@yahoo.com,"
    ; "thebarbali@hotmail.com,"
    ; "trustbali@balidream.com,"
    ; "teraoka@his-bali.com,"
    ; "candle@dps.centrin.net.id,"
    ; "waterbom@denpasar.wasantara.net.id,"
    ; "ib.suparsa@yahoo.com,"
    ; "budhipra@nesiancea.com,"
    ; "info@kindvillabintang.com,"
    ; "pch@novotelbali.com,"
    ; "parigata@indosat.net.id,"
    ; "mail@grandmirage.com,"
    ; "ananda_resort@hotmail.com,"
    ; "info@risatabali.com,"
    ; "gwkbali@indosat.net.id,"
    ; "rai@gosharestaurant.com,"
    ; "santika@santikabali.com,"
    ; "sahidbl@indosat.net.id,"
    ; "tubanrestaurant@yahoo.com,"
    ; "sales@thejimbaranbali.com,"
    ; "info@thejimbaranbali.com,"
    ; "sari@bubbagumpbali.com,"
    ; "Winnie@grandlingga.com,"
    ; "juaidy_asia@yahoo.com,"
    ; "vicmgr@i-xplore.com,"
    ; "langka@theclubstore.co.id,"
    ; "lilakresna@ConradBali.com,"
    ; "wayan.atmaja@luxurycollecton.com,"
    ; "Cisabali@indo.net.id,"
    ; "garrant@indo.net.id,"
    ; "owenwister@yahoo.com,"
    ; "tiara@dps.mega.net.id,"
    ; "info@nzmuslim.net,"
    ; "yuanito.kurniawan@sea.ccamatil.com,"
    ; "pitamaha@indosat.net.id,"
    ; "yunani@theclubstore.co.id,"
    ; "deklis@hotmail.com,"
    ; "cianjur@indo.net.id,"
    ; "mahajayatower@hotmail.com,"
    ; "endra@centrin.net.id,"
    ; "wayan.dirayana@fourseasons.com,"
    ; "balinaga@dps.centrin.net.id,"
    ; "tiaradwt@dps.centrin.net.id,"
    ; "candrator@hotmail.com,"
    ; "altaraspa@yahoo.com,"
    ; "fani@clubbali.com,"
    ; "Itudm@dps.centrin.net.id,"
    ; "baliratuspa@biz.net.id,"
    ; "kawasspa@indosat.net.id,"
    ; "hatoe7@yahoo.co.jp,"
    ; "sales@mimpi.com,"
    ; "theroyal@indosat.net.id,"
    ; "chakra_92@yahoo.com,"
    ; "u_dmtdps@sosro.com,"
    ; "januar@citramedia.net,"
    ; "januar@balivisioncomp.com,"
    ; "admin@balivisioncomp.com,"
    ; "ansri@dps.mega.net.id,"
    ; "info@rijasaresort-villas.com,"
    ; "sales@komaneka.com,"
    ; "multigun@indo.net.id,"
    ; "ishwari@bagus-discovery.com,"
    ; "utami@bali-exoticwedding.com,"
    ; "putra_wirata@hotmail.com,"
    ; "arte@dps.centrin.net.id,"
    ; "hamiludd2kwah@yahoo.com.au,"
    ; "btu_cipluk@yahoo.com,"
    ; "agus@indo-journey.com,"
    ; "agus.winarko@gmail.com,"
    ; "agus.amirudin@wilmar.co.id,"
    ; "adamsilver@lycos.com,"
    ; "yayasanlaroyba@yahoo.co.id,"
    ; "luminaABC@hotmail.com,"
    ; "umasapna@coconuthomes.com,"
    ; "udsupradinasty@yahoo.co.id,"
    ; "ticketing@bagus-discovery.com,"
    ; "tejo@pttropical.co.id,"
    ; "syamklw@yahoo.com,"
    ; "sutiarso21@yahoo.com,"
    ; "silvia_maniz@yahoo.com,"
    ; "yenny_kurniawaty@telkomsel.co.id,"
    ; "lega@kramatdjatigroup.com,"
    ; "stadiumcafe@indonet.id,"
    ; "agencyfreestylebali@yahoo.com,"
    ; "yayaqdarma@yahoo.co.id,"
    ; "hanafiid@yahoo.com,"
    ; "ricky_dvt@yahoo.co.id,"
    ; "teuku_umar@binus-centre.com,"
    ; "flp_bali@yahoo.com,"
    ; "andy@ritzcarlton-bali.com,"
    ; "bapakbakery@dps.centrin.net.id,"
    ; "siddiq@teacher.com,"
    ; "clipper@indo.net.id,"
    ; "puricendana@yahoo.com,"
    ; "info@ripcurlschoolsurf.com,"
    ; "sales@ramabeachhotel.com,"
    ; "healing@indosat.net.id,"
    ; "djinaldi@yahoo.co.uk,"
    ; "rotary.bali.kuta@gmail.com,"
    ; "dadang@ma-joly.com,"
    ; "takenoko_bali@yahoo.co.id,"
    ; "hrd@novotelbali.com,"
    ; "purwa@kcb-tours.com,"
    ; "anggie.gendut@england.com,"
    ; "novyog@indo.net.id,"
    ; "reservation@meliabali.com,"
    ; "sales@meliabali.com,"
    ; "info@rkeconsulting.com,"
    ; "andisetiaji@abacus-ind.co.id,"
    ; "sales.corp@swissgrandbali.com,"
    ; "karsana.wirajaya@trac.astra.co.id,"
    ; "muliatr@indosat.net.id,"
    ; "nita@surfer-girl.com,"
    ; "diah.permana@bagus-discovery.com,"
    ; "purwabali@yahoo.com,"
    ; "oly@islandconcpets.com,"
    ; "info@islandconcepts.com,"
    ; "gag@indo.net.id,"
    ; "gkumala@indosat.net.id,"
    ; "thegardeniavillas@meliabali.com,"
    ; "purchasing.mgr@thelegianbali.com,"
    ; "info@paradisebaliholidays.com,"
    ; "agus.winarko@bagus-discovery.com,"
    ; "cozytimes26@yahoo.com,"
    ; "info@papua-adventures.com,"
    ; "lokasaribali@hotmail.com,"
    ; "wahana@baliforyou.com,"
    ; "Stephen@victuslife.com,"
    ; "operations@atlasbalitours.com,"
    ; "balicoffeeshop@hotmail.com,"
    ; "mayakutacentre@telkom.net,"
    ; "rikmawan@dps.centrin.net.id,"
    ; "ndbt@bagus-discovery.com,"
    ; "info@indographs.com,"
    ; "aridwan_sgb@yahoo.com,"
    ; "bali@atmosphere.co.id,"
    ; "plmgrd@indosat.net.id,"
    ; "balibless@padmaubud.biz,"
    ; "baliaura@yahoo.com,"
    ; "andalan@bali.net,"
    ; "dmandiri@indo.net.id,"
    ; "pernadi@rad.net.id,"
    ; "Tabetha@BeyondMenus.com,"
    ; "adityafood@yahoo.com,"
    ; "sarana_com@yahoo.com,"
    ; "pasadena@chek.com," ] in
  let reporter = Logs.reporter () in
  List.iteri (fun v k ->
    Part.insert mmu k v ;
    Logs.set_reporter Logs.nop_reporter ;
    Fmt.epr "%a\n%!" Part.pp mmu ;
    Logs.set_reporter reporter) elts ;
  let reporter = Logs.reporter () in
  Logs.set_reporter Logs.nop_reporter ;
  Fmt.epr "%a\n%!" Part.pp mmu ;
  Logs.set_reporter reporter ;
  Part.insert mmu "sales@pica-pica.com," (-1) ;
  Logs.set_reporter Logs.nop_reporter ;
  Fmt.epr "%a\n%!" Part.pp mmu ;
  Logs.set_reporter reporter ;
  Alcotest.(check int) "sales@pica-pica.com," (Part.lookup mmu "sales@pica-pica.com,") (-1) ;
  List.iteri (fun v' k -> Alcotest.(check int) k (Part.lookup mmu k) v') elts ;
;;

let test09 =
  Alcotest.test_case "test09" `Quick @@ fun _file ->
  let index = Rresult.R.get_ok (Bos.OS.File.tmp "index-%s") in
  create (Fpath.to_string index) ;
  let mmu = mmu_of_file (Fpath.to_string index) in
  let elts =
    [ "adhidharma@denpasar.wasantara.net.id"
    ; "centralreservation@ramayanahotel.com"
    ; "apribadi@balimandira.com"
    ; "cdagenhart@ifc.org"
    ; "dana_supriyanto@interconti.com"
    ; "dos@novotelbali.com"
    ; "daniel@hotelpadma.com"
    ; "daniel@balibless.com"
    ; "djoko_p@jayakartahotelsresorts.com"
    ; "expdepot@indosat.net.id"
    ; "feby.adamsyah@idn.xerox.com"
    ; "christian_rizal@interconti.com"
    ; "singgih93@mailcity.com"
    ; "idonk_gebhoy@yahoo.com"
    ; "info@houseofbali.com"
    ; "kyohana@toureast.net"
    ; "sales@nusaduahotel.com"
    ; "jayakarta@mataram.wasantara.net.id"
    ; "mapindo@indo.net.id"
    ; "sm@ramayanahotel.com"
    ; "anekabeach@dps.centrin.net.id"
    ; "yogya@jayakartahotelsresorts.com"
    ; "garudawisatajaya@indo.net.id"
    ; "ketut@kbatur.com"
    ; "bondps@bonansatours.com"
    ; "witamgr@dps.centrin.net.id"
    ; "dtedja@indosat.net.id"
    ; "info@stpbali.ac.id"
    ; "baliprestigeho@dps.centrin.net.id"
    ; "pamilu@mas-travel.com"
    ; "amandabl@indosat.net.id"
    ; "marketing@csdwholiday.com"
    ; "luha89@yahoo.com"
    ; "indahsuluh2002@yahoo.com.sg"
    ; "imz1991@yahoo.com"
    ; "gus_war81@yahoo.com"
    ; "kf034@indosat.net.id"
    ; "800produkwil@posindonesia.co.id"
    ; "kontak.synergi@yahoo.com"
    ; "oekaoeka@yahoo.com"
    ; "fitrianti@hotmail.com"
    ; "meylina310@yahoo.com"
    ; "h4ntoro@yahoo.com"
    ; "novi_enbe@yahoo.com"
    ; "dila_dewata@yahoo.co.id"
    ; "tiena_asfary@yahoo.co.id"
    ; "da_lawoffice@yahoo.com"
    ; "rini@ncsecurities.biz"
    ; "sudarnoto_hakim@yahoo.com"
    ; "wastioke@yahoo.com"
    ; "leebahri@yahoo.com."
    ; "lia_kiara97@yahoo.com"
    ; "rido@weddingku.com"
    ; "b_astuti@telkomsel.co.id"
    ; "garudawisata@indo.net.id"
    ; "grfurniture@yahoo.com"
    ; "gosyen2000@hotmail.com"
    ; "hvhfood@indosat.net.id"
    ; "hr@astonbali.com"
    ; "hary@wibisono-family.com"
    ; "fadlycak'p@yahoo.com"
    ; "ida_sampurniah@telkomsel.co.id"
    ; "muslim-pariwisata-bali@yahoogroups.com"
    ; "harisnira@yahoo.com"
    ; "sales@houseofbali.com"
    ; "baim_ron@yahoo.com"
    ; "ilhambali222@yahoo.com"
    ; "bungjon@gmail.com"
    ; "diar@bdg.centrin.net.id"
    ; "elmienruge@hotmail.com"
    ; "galaxygarden2006@yahoo.com"
    ; "gorisata@indosat.net.id"
    ; "maulitasarihani@yahoo.com"
    ; "hamiluddakwah@gmail.com.au"
    ; "bounty@indo.net.id,"
    ; "michi@ritzcarlton-bali.com,"
    ; "orridor@dps.centrin.net.id,"
    ; "ngumina@hotmail.com,"
    ; "made@mas-travel.com,"
    ; "evi@mas-travel.com,"
    ; "wibawa@mas-travel.com,"
    ; "saihubaly@yahoo.co.id,"
    ; "swa_candra@yahoo.com,"
    ; "picapica@denpasar.wasantara.net.id,"
    ; "griyasantrian@santrian.com,"
    ; "yuni6671@gmail.com,"
    ; "phbalichef@indo.net.id,"
    ; "vendra@keratonjimbaranresort.com,"
    ; "bali@pansea.com,"
    ; "sales@legianbeachbali.com,"
    ; "purchasing@meliabali.com,"
    ; "swacandra@telkom.net,"
    ; "lysbeth@paintballbali.com,"
    ; "trvlindo@upg.mega.net.id,"
    ; "lim_thefaith@yahoo.com,"
    ; "uungtb@yahoo.com.au,"
    ; "vivaldil307@hotmail.com,"
    ; "iodakon@yahoo.co.id,"
    ; "reservation@pendawahotel.com,"
    ; "ptbon@dps.centrin.net.id,"
    ; "ptlamak@indosat.net.id,"
    ; "sculpt@indo.net.id,"
    ; "memedi-gwkbali@dps.centrin.net.id,"
    ; "info@leisuredream.com,"
    ; "indra_wijaya@hero.co.id,"
    ; "ndbconvex@bagus-discovery.com,"
    ; "Endro@bma-merdeka.com,"
    ; "wsuardana@indosat.net.id,"
    ; "bali@esmirada.com,"
    ; "BAL.Purchasing@fourseasons.com,"
    ; "ruby@marthatilaar-spa.com,"
    ; "villaseminyak@eksadata.com,"
    ; "sariati@sanurbeach.aerowisata.com,"
    ; "info@jenggala-bali.com,"
    ; "chef@nusaduahotel.com,"
    ; "info@balicateringcompany.com,"
    ; "moka@dps.mega.net.id,"
    ; "zsa@eyeview.info,"
    ; "winarios@indosat.net.id,"
    ; "project@balihai-rsort.com,"
    ; "vivi@kopibali.com,"
    ; "peninsulabali@dps.centrin.net.id,"
    ; "ust.july@mas-travel.com,"
    ; "ubud@pansea.com,"
    ; "ustad_july@yahoo.com,"
    ; "thebarbali@hotmail.com,"
    ; "trustbali@balidream.com,"
    ; "teraoka@his-bali.com,"
    ; "candle@dps.centrin.net.id,"
    ; "waterbom@denpasar.wasantara.net.id,"
    ; "ib.suparsa@yahoo.com,"
    ; "budhipra@nesiancea.com,"
    ; "info@kindvillabintang.com,"
    ; "pch@novotelbali.com,"
    ; "parigata@indosat.net.id,"
    ; "mail@grandmirage.com,"
    ; "ananda_resort@hotmail.com,"
    ; "info@risatabali.com,"
    ; "gwkbali@indosat.net.id,"
    ; "rai@gosharestaurant.com,"
    ; "santika@santikabali.com,"
    ; "sahidbl@indosat.net.id,"
    ; "tubanrestaurant@yahoo.com,"
    ; "sales@thejimbaranbali.com,"
    ; "info@thejimbaranbali.com,"
    ; "sari@bubbagumpbali.com,"
    ; "Winnie@grandlingga.com,"
    ; "juaidy_asia@yahoo.com,"
    ; "vicmgr@i-xplore.com,"
    ; "langka@theclubstore.co.id,"
    ; "lilakresna@ConradBali.com,"
    ; "wayan.atmaja@luxurycollecton.com,"
    ; "Cisabali@indo.net.id,"
    ; "garrant@indo.net.id,"
    ; "owenwister@yahoo.com,"
    ; "tiara@dps.mega.net.id,"
    ; "info@nzmuslim.net,"
    ; "yuanito.kurniawan@sea.ccamatil.com,"
    ; "pitamaha@indosat.net.id,"
    ; "yunani@theclubstore.co.id,"
    ; "deklis@hotmail.com,"
    ; "cianjur@indo.net.id,"
    ; "mahajayatower@hotmail.com,"
    ; "endra@centrin.net.id,"
    ; "wayan.dirayana@fourseasons.com,"
    ; "balinaga@dps.centrin.net.id,"
    ; "tiaradwt@dps.centrin.net.id,"
    ; "candrator@hotmail.com,"
    ; "altaraspa@yahoo.com,"
    ; "fani@clubbali.com,"
    ; "Itudm@dps.centrin.net.id,"
    ; "baliratuspa@biz.net.id,"
    ; "kawasspa@indosat.net.id,"
    ; "hatoe7@yahoo.co.jp,"
    ; "sales@mimpi.com,"
    ; "theroyal@indosat.net.id,"
    ; "chakra_92@yahoo.com,"
    ; "u_dmtdps@sosro.com,"
    ; "januar@citramedia.net,"
    ; "januar@balivisioncomp.com,"
    ; "admin@balivisioncomp.com,"
    ; "ansri@dps.mega.net.id,"
    ; "info@rijasaresort-villas.com,"
    ; "sales@komaneka.com,"
    ; "multigun@indo.net.id,"
    ; "ishwari@bagus-discovery.com,"
    ; "utami@bali-exoticwedding.com,"
    ; "putra_wirata@hotmail.com,"
    ; "arte@dps.centrin.net.id,"
    ; "hamiludd2kwah@yahoo.com.au,"
    ; "btu_cipluk@yahoo.com,"
    ; "agus@indo-journey.com,"
    ; "agus.winarko@gmail.com,"
    ; "agus.amirudin@wilmar.co.id,"
    ; "adamsilver@lycos.com,"
    ; "yayasanlaroyba@yahoo.co.id,"
    ; "luminaABC@hotmail.com,"
    ; "umasapna@coconuthomes.com,"
    ; "udsupradinasty@yahoo.co.id,"
    ; "ticketing@bagus-discovery.com,"
    ; "tejo@pttropical.co.id,"
    ; "syamklw@yahoo.com,"
    ; "sutiarso21@yahoo.com,"
    ; "silvia_maniz@yahoo.com,"
    ; "yenny_kurniawaty@telkomsel.co.id,"
    ; "lega@kramatdjatigroup.com,"
    ; "stadiumcafe@indonet.id,"
    ; "agencyfreestylebali@yahoo.com,"
    ; "yayaqdarma@yahoo.co.id,"
    ; "hanafiid@yahoo.com,"
    ; "ricky_dvt@yahoo.co.id,"
    ; "teuku_umar@binus-centre.com,"
    ; "flp_bali@yahoo.com,"
    ; "andy@ritzcarlton-bali.com,"
    ; "bapakbakery@dps.centrin.net.id,"
    ; "siddiq@teacher.com,"
    ; "clipper@indo.net.id,"
    ; "puricendana@yahoo.com,"
    ; "info@ripcurlschoolsurf.com,"
    ; "sales@ramabeachhotel.com,"
    ; "healing@indosat.net.id,"
    ; "djinaldi@yahoo.co.uk,"
    ; "rotary.bali.kuta@gmail.com,"
    ; "dadang@ma-joly.com,"
    ; "takenoko_bali@yahoo.co.id,"
    ; "hrd@novotelbali.com,"
    ; "purwa@kcb-tours.com,"
    ; "anggie.gendut@england.com,"
    ; "novyog@indo.net.id,"
    ; "reservation@meliabali.com,"
    ; "sales@meliabali.com,"
    ; "info@rkeconsulting.com,"
    ; "andisetiaji@abacus-ind.co.id,"
    ; "sales.corp@swissgrandbali.com,"
    ; "karsana.wirajaya@trac.astra.co.id,"
    ; "muliatr@indosat.net.id,"
    ; "nita@surfer-girl.com,"
    ; "diah.permana@bagus-discovery.com,"
    ; "purwabali@yahoo.com,"
    ; "oly@islandconcpets.com,"
    ; "info@islandconcepts.com,"
    ; "gag@indo.net.id,"
    ; "gkumala@indosat.net.id,"
    ; "thegardeniavillas@meliabali.com,"
    ; "purchasing.mgr@thelegianbali.com,"
    ; "info@paradisebaliholidays.com,"
    ; "agus.winarko@bagus-discovery.com,"
    ; "cozytimes26@yahoo.com,"
    ; "info@papua-adventures.com,"
    ; "lokasaribali@hotmail.com,"
    ; "wahana@baliforyou.com,"
    ; "Stephen@victuslife.com,"
    ; "operations@atlasbalitours.com,"
    ; "balicoffeeshop@hotmail.com,"
    ; "mayakutacentre@telkom.net,"
    ; "rikmawan@dps.centrin.net.id,"
    ; "ndbt@bagus-discovery.com,"
    ; "info@indographs.com,"
    ; "aridwan_sgb@yahoo.com,"
    ; "bali@atmosphere.co.id,"
    ; "plmgrd@indosat.net.id,"
    ; "balibless@padmaubud.biz,"
    ; "baliaura@yahoo.com,"
    ; "andalan@bali.net,"
    ; "dmandiri@indo.net.id,"
    ; "pernadi@rad.net.id,"
    ; "Tabetha@BeyondMenus.com,"
    ; "adityafood@yahoo.com,"
    ; "sarana_com@yahoo.com,"
    ; "pasadena@chek.com,"
    ; "sales@pica-pica.com,"
    ; "menara_fbi@hotmail.com,"
    ; "home_treasure@hotmail.com,"
    ; "aamsalim@dps.centrin.net.id,"
    ; "shell_enoproduction@yahoo.com,"
    ; "geckoleather@hotmail.com,"
    ; "milagro_bali@hotmail.com,"
    ; "gemini19id@yahoo.com,"
    ; "karyacargo@dps.centrin.net.id,"
    ; "darabali@indo.net.id,"
    ; "padiprada@hotmail.com,"
    ; "vijowiz@yahoo.com,"
    ; "cafejimbaran@mekarsaribali.com,"
    ; "isnamks@yahoo.com,"
    ; "sales@allseasonslegian.com,"
    ; "chitra@cangguclub.com,"
    ; "cheriaM@xl.co.id,"
    ; "geo-trek@dps.centrin.net.id,"
    ; "sales@balipasadena.com,"
    ; "sales@villahening.com,"
    ; "fc@novotelbali.com,"
    ; "maolbing83@yahoo.co.id,"
    ; "info@dimensitropika.com,"
    ; "news@tabloidpiknik.com,"
    ; "mediacentre@bali-tourism.com,"
    ; "bioland-bali@telkom.net,"
    ; "glf-bali@indo.net.id,"
    ; "info@asiabali.com,"
    ; "takanit@yahoo.com,"
    ; "jamal@hrbc-bali.co.id,"
    ; "naniek@alilahotels.com,"
    ; "ndbtdps@dps.mega.net.id,"
    ; "mbcbali_jaka@yahoo.com,"
    ; "masnyonya@telkom.net,"
    ; "merrystravel@dps.centrin.net.id,"
    ; "mail@baliintermedia.com,"
    ; "mitrakridamandiri@hotmail.com,"
    ; "kartikaplz@denpasar.wasantara.net.id,"
    ; "oedps@indosat.net.id,"
    ; "jalirest@indosat.net.id,"
    ; "jenni_hartatik@interconti.com,"
    ; "info@alamkulkul.com,"
    ; "info@aggacitta.com,"
    ; "info@jasatours.com,"
    ; "iskandar.Liemena@idn.xerox.com,"
    ; "info@lorinresortsababai.com,"
    ; "ketutsukarta@telkom.net,"
    ; "renata.hutasoit@hyattintl.com,"
    ; "sukiato@hotelpadma.com,"
    ; "salesser@idola.net.id,"
    ; "sales@bali-clubaqua.com,"
    ; "sales@amandaresort.com,"
    ; "sales@balimandira.com," ] in
  let reporter = Logs.reporter () in
  List.iteri (fun v k ->
    Part.insert mmu k v ;
    Logs.set_reporter Logs.nop_reporter ;
    Fmt.epr "%a\n%!" Part.pp mmu ;
    Logs.set_reporter reporter) elts ;
  let reporter = Logs.reporter () in
  Logs.set_reporter Logs.nop_reporter ;
  Fmt.epr "%a\n%!" Part.pp mmu ;
  Logs.set_reporter reporter ;
  Part.insert mmu "reservation@ramacandidasahotel.com," (-1) ;
  Logs.set_reporter Logs.nop_reporter ;
  Fmt.epr "%a\n%!" Part.pp mmu ;
  Logs.set_reporter reporter ;
  Alcotest.(check int) "sales@pica-pica.com," (Part.lookup mmu "reservation@ramacandidasahotel.com,") (-1) ;
  List.iteri (fun v' k -> Alcotest.(check int) k (Part.lookup mmu k) v') elts ;
;;

open Cmdliner

let filename =
  let parser x = match Fpath.of_string x with
    | Ok v when not (Sys.file_exists x) -> create (Fpath.to_string v) ; Ok (mmu_of_file (Fpath.to_string v))
    | Ok v -> Rresult.R.error_msgf "%a already exists" Fpath.pp v
    | Error _ as err -> err in
  let pp ppf _ = Fmt.pf ppf "#index" in
  Arg.conv (parser, pp)

let filename =
  let doc = "The persistent index file." in
  Arg.(value & opt (some filename) None & info [ "index" ] ~doc)

let () = Alcotest.run_with_args "rowex" filename
    [ "simple", [ test01; test02; test03; test04; test05; test06; test07; test08; test09 ] ]
