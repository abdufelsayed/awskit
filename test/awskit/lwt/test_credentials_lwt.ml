module Provider = Awskit_lwt.Credentials.Provider

let credentials =
  Awskit.Credentials.create_exn ~access_key_id:"AKID"
    ~secret_access_key:"SECRET" ()

let resolve provider = Provider.resolve provider |> Lwt_main.run

let test_chain_continues_only_on_unavailable () =
  let unavailable =
    Provider.create (fun () ->
        Lwt.return
          (Provider.Unavailable
             { source = `Env; reason = "environment not configured" }))
  in
  let later_called = ref false in
  let resolved =
    Provider.create (fun () ->
        later_called := true;
        Lwt.return (Provider.Resolved credentials))
  in
  (match resolve (Provider.chain [ unavailable; resolved ]) with
  | Provider.Resolved actual ->
      Alcotest.(check string)
        "resolved access key" "AKID"
        (Awskit.Credentials.access_key_id actual)
  | Unavailable _ | Invalid _ | Failed _ ->
      Alcotest.fail "unavailable provider should fall through");
  Alcotest.(check bool) "later provider called" true !later_called;
  let invalid_error = Awskit.Error.Producer.credentials "invalid source" in
  later_called := false;
  let invalid =
    Provider.create (fun () -> Lwt.return (Provider.Invalid invalid_error))
  in
  (match resolve (Provider.chain [ invalid; resolved ]) with
  | Provider.Invalid error ->
      Alcotest.(check bool)
        "invalid preserved" true
        (Awskit.Error.equal invalid_error error)
  | Resolved _ | Unavailable _ | Failed _ ->
      Alcotest.fail "invalid provider must stop the chain");
  Alcotest.(check bool) "later provider not called" false !later_called

let test_native_cancellation_is_preserved () =
  let provider = Provider.create (fun () -> Lwt.fail Lwt.Canceled) in
  match resolve provider with
  | exception Lwt.Canceled -> ()
  | _ -> Alcotest.fail "provider cancellation must remain Lwt.Canceled"

let () =
  Alcotest.run "awskit-lwt-credentials"
    [
      ( "unit:awskit-lwt:credentials",
        [
          Alcotest.test_case "chain continues only on unavailable" `Quick
            test_chain_continues_only_on_unavailable;
          Alcotest.test_case "native cancellation is preserved" `Quick
            test_native_cancellation_is_preserved;
        ] );
    ]
