let contains text substring =
  let text_length = String.length text in
  let substring_length = String.length substring in
  let rec loop index =
    if index + substring_length > text_length then false
    else if String.equal (String.sub text index substring_length) substring then
      true
    else loop (index + 1)
  in
  substring_length = 0 || loop 0

let test_denied_bind_skips_by_default () =
  match Loopback_policy.bind_denied_action ~require_loopback:false with
  | Skip -> ()
  | Fail message ->
      Alcotest.failf "expected skip action, got failure: %s" message

let test_denied_bind_fails_when_required () =
  match Loopback_policy.bind_denied_action ~require_loopback:true with
  | Fail message ->
      Alcotest.(check bool)
        "mentions runtime HTTP loopback requirement" true
        (contains message Loopback_policy.require_loopback_env)
  | Skip -> Alcotest.fail "expected failure action"

exception Preflight_denied

let test_preflight_runs_before_generated_case () =
  let calls = ref [] in
  let generated_case =
    Loopback_policy.with_loopback_preflight
      ~ensure_loopback_available:(fun () -> calls := !calls @ [ "preflight" ])
      ( "generated scenarios",
        `Quick,
        fun () -> calls := !calls @ [ "generated" ] )
  in
  let _, _, run = generated_case in
  run ();
  Alcotest.(check (list string))
    "call order"
    [ "preflight"; "generated" ]
    !calls

let test_preflight_stops_generated_case () =
  let generated_ran = ref false in
  let generated_case =
    Loopback_policy.with_loopback_preflight
      ~ensure_loopback_available:(fun () -> raise Preflight_denied)
      ("generated scenarios", `Quick, fun () -> generated_ran := true)
  in
  let _, _, run = generated_case in
  match run () with
  | exception Preflight_denied ->
      Alcotest.(check bool) "generated case did not run" false !generated_ran
  | () -> Alcotest.fail "expected preflight exception"

let suite =
  [
    ( "contract:awskit-test:runtime-http-loopback",
      [
        Alcotest.test_case "sandbox bind skips by default" `Quick
          test_denied_bind_skips_by_default;
        Alcotest.test_case "required loopback bind fails" `Quick
          test_denied_bind_fails_when_required;
        Alcotest.test_case "preflight runs before generated case" `Quick
          test_preflight_runs_before_generated_case;
        Alcotest.test_case "preflight stops generated case" `Quick
          test_preflight_stops_generated_case;
      ] );
  ]

let () = Alcotest.run "awskit-runtime-http-loopback-policy" suite
