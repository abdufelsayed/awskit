include Awskit_lwt.Make (Cohttp_lwt_unix.Client)

let create ?ctx ?scheme ~region ~credentials ?(clock = Ptime_clock.now)
    ?endpoint ?port () =
  create ?ctx ?scheme ~region ~credentials ~clock ?endpoint ?port ()
