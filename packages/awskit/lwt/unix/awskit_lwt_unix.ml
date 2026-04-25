module Strict = Awskit_lwt.Make (Cohttp_lwt_unix.Client)
include Strict

let create ?ctx ?endpoint ?region ?credentials ?(clock = Ptime_clock.now)
    ?max_response_body_bytes () =
  match
    ( (match region with
      | Some region -> Awskit.Region.of_string region
      | None -> Awskit_unix.Region.from_env ()),
      match credentials with
      | Some credentials -> Ok credentials
      | None -> Awskit_unix.Credentials.from_env () )
  with
  | Ok region, Ok credentials ->
      Ok
        (Strict.create ?ctx ?endpoint ~region ~credentials ~clock
           ?max_response_body_bytes ())
  | Error error, _ | _, Error error -> Error error
