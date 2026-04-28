module Strict = Awskit_lwt.Make (Cohttp_lwt_unix.Client)
include Strict

let create ?ctx ?endpoint ?region ?credentials ?(clock = Ptime_clock.now)
    ?retry_policy ?max_response_body_bytes () =
  match
    ( (match region with
      | Some region -> Awskit.Region.of_string region
      | None -> Awskit_unix.Region.from_env ()),
      match credentials with
      | Some credentials -> Ok credentials
      | None -> Awskit_unix.Credentials.from_env () )
  with
  | Ok region, Ok credentials ->
      let sleep span = Lwt_unix.sleep (Ptime.Span.to_float_s span) in
      Ok
        (Strict.create ?ctx ?endpoint ~region ~credentials ~clock ?retry_policy
           ~sleep ?max_response_body_bytes ())
  | Error error, _ | _, Error error -> Error error
