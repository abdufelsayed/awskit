(** Property-based tests: buggify consistency, CAS races. *)

open Awskit_s3

let bucket = "test-bucket"

let make_sim () =
  let clock = Sim.Clock.create () in
  let store = Sim.create_store ~clock () in
  let creds =
    Awskit.Credentials.make ~access_key_id:"admin" ~secret_access_key:"s" ()
  in
  let c = Sim.connect store ~credentials:creds in
  ignore (Sim.Bucket.create c ~bucket ());
  (store, c)

(* ── Buggify never corrupts ─────────────────────────────────────── *)

let test_buggify_consistency =
  QCheck.Test.make ~count:2000 ~name:"buggify never corrupts store"
    QCheck.(
      pair int
        (list_size (Gen.int_range 1 20)
           (pair
              (Gen.map (fun s -> "k/" ^ s) Gen.string_small |> make)
              string_small)))
    (fun (seed, ops) ->
      let _store, c = make_sim () in
      Sim.enable_buggify c ~seed ~prob:0.3;
      List.iter
        (fun (key, value) -> ignore (Sim.Object.put c ~bucket ~key value))
        ops;
      (* Disable buggify for verification *)
      Sim.disable_buggify c;
      (* The store must be internally consistent: every successful get
         returns a value that was actually written to that key *)
      let all_values_for key =
        List.filter_map
          (fun (k, v) -> if String.equal k key then Some v else None)
          ops
      in
      let keys_seen = Hashtbl.create 16 in
      List.iter (fun (k, _) -> Hashtbl.replace keys_seen k true) ops;
      Hashtbl.fold
        (fun key _ acc ->
          acc
          &&
          match Sim.Object.get c ~bucket ~key () with
          | Ok r ->
              List.exists
                (String.equal r.Object.Get_result.body)
                (all_values_for key)
          | Error `Not_found -> true (* never successfully written *)
          | Error _ -> false)
        keys_seen true)

(* ── CAS race: exactly one winner ───────────────────────────────── *)

let test_cas_race =
  QCheck.Test.make ~count:500 ~name:"concurrent CAS: exactly one winner"
    QCheck.(int_range 2 8)
    (fun n ->
      let store, _admin = make_sim () in
      let clients =
        List.init n (fun i ->
            let creds =
              Awskit.Credentials.make ~access_key_id:(Fmt.str "client-%d" i)
                ~secret_access_key:"s" ()
            in
            Sim.connect store ~credentials:creds)
      in
      let key = "cas-key" in
      let results =
        List.mapi
          (fun i c ->
            Sim.Object.put c ~bucket ~key ~if_none_match:true (Fmt.str "v%d" i))
          clients
      in
      let ok_count =
        List.fold_left
          (fun acc r -> if Result.is_ok r then acc + 1 else acc)
          0 results
      in
      ok_count = 1)

let suite =
  [
    ( "pbt:faults:buggify",
      [ QCheck_alcotest.to_alcotest test_buggify_consistency ] );
    ("pbt:faults:cas-race", [ QCheck_alcotest.to_alcotest test_cas_race ]);
  ]
