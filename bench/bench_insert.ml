open Bechamel
open Toolkit

let ( <.> ) f g x = f (g x)
let random_max = 32767.

let random_normal n =
  let m = n + (n mod 2) in
  let values = Array.create_float n in
  for i = 0 to (m / 2) - 1 do
    let x = ref 0. and y = ref 0. and rsq = ref 0. in
    while
      x := (Random.float random_max /. random_max *. 2.0) -. 1.;
      y := (Random.float random_max /. random_max *. 2.0) -. 1.;
      rsq := (!x *. !x) +. (!y *. !y);
      !rsq >= 1. || !rsq = 0.
    do
      ()
    done;
    let f = sqrt (-2.0 *. log !rsq /. !rsq) in
    values.(i * 2) <- !x *. f;
    values.((i * 2) + 1) <- !y *. f
  done;
  Array.map (abs <.> Float.to_int <.> ( *. ) random_max) values

let random_string ln =
  let rs = Bytes.create ln in
  let ic = open_in "/dev/urandom" in
  really_input ic rs 0 ln;
  close_in ic;
  for i = 0 to ln - 1 do
    if Bytes.get rs i = '\000' then Bytes.set rs i '\001'
  done;
  Bytes.unsafe_to_string rs

let db = Array.map (fun v -> (random_string v, v)) (random_normal 1000)

let test0 =
  Test.make ~name:"art" @@ Staged.stage
  @@ fun () ->
  let tree = Art.make () in
  Array.iter (fun (k, v) -> Art.insert tree (Art.unsafe_key k) v) db

let test1 =
  Test.make ~name:"hashtbl" @@ Staged.stage
  @@ fun () ->
  let tbl = Hashtbl.create 0x100 in
  Array.iter (fun (k, v) -> Hashtbl.add tbl k v) db

let test = Test.make_grouped ~name:"insert" [ test0; test1 ]

let benchmark () =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |]
  in
  let instances =
    Instance.[ minor_allocated; major_allocated; monotonic_clock ]
  in
  let cfg =
    Benchmark.cfg ~limit:3000 ~quota:(Time.second 2.0) ~kde:(Some 1000) ()
  in
  let raw_results = Benchmark.all cfg instances test in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  let results = Analyze.merge ols instances results in
  (results, raw_results)

let nothing _ = Ok ()

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window
    ~predictor:Measure.run results

let () =
  let results = benchmark () in
  match Sys.argv with
  | [| _; "cli" |] ->
      let open Notty_unix in
      List.iter
        (fun v -> Bechamel_notty.Unit.add v (Measure.unit v))
        Instance.[ minor_allocated; major_allocated; monotonic_clock ];
      let window =
        match winsize Unix.stdout with
        | Some (w, h) -> { Bechamel_notty.w; h }
        | None -> { Bechamel_notty.w = 80; h = 1 }
      in
      let results, _ = benchmark () in
      img (window, results) |> eol |> output_image
  | [| _; "json" |] | _ ->
      let results =
        let open Bechamel_js in
        emit ~dst:(Channel stdout) nothing ~compare:String.compare
          ~x_label:Measure.run
          ~y_label:(Measure.label Instance.monotonic_clock)
          results
      in
      Rresult.R.failwith_error_msg results
