include Awskit_s3_lwt.Make (Cohttp_lwt_unix.Client)

let create ?ctx ?endpoint ~region ~credentials ?(clock = Ptime_clock.now)
    ?max_response_body_bytes () =
  create ?ctx ?endpoint ~region ~credentials ~clock ?max_response_body_bytes ()
