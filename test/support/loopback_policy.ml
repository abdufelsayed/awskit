type bind_denied_action = Skip | Fail of string

let require_loopback_env = "AWSKIT_RUNTIME_HTTP_REQUIRE_LOOPBACK"

let require_loopback_from_env () =
  match Sys.getenv_opt require_loopback_env with
  | Some ("1" | "true" | "TRUE" | "yes" | "YES") -> true
  | None | Some _ -> false

let bind_denied_action ~require_loopback =
  if require_loopback then
    Fail
      (Printf.sprintf
         "local loopback listener bind was denied; %s=1 requires runtime HTTP \
          workload evidence instead of a sandbox skip"
         require_loopback_env)
  else Skip

let handle_bind_denied () =
  match bind_denied_action ~require_loopback:(require_loopback_from_env ()) with
  | Skip -> Alcotest.skip ()
  | Fail message -> Alcotest.fail message

let with_loopback_preflight ~ensure_loopback_available
    ((name, speed_level, run) : unit Alcotest.test_case) =
  ( name,
    speed_level,
    fun () ->
      ensure_loopback_available ();
      run () )
