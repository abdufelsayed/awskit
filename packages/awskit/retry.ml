type t = {
  max_attempts : int;
  base_delay : Ptime.Span.t;
  max_delay : Ptime.Span.t;
  jitter : float;
}

let span_exn seconds =
  match Ptime.Span.of_float_s seconds with
  | Some span -> span
  | None -> invalid_arg "Awskit.Retry: invalid span"

let default_base_delay = span_exn 0.1
let default_max_delay = span_exn 2.0

let create ?(max_attempts = 3) ?(base_delay = default_base_delay)
    ?(max_delay = default_max_delay) ?(jitter = 0.0) () =
  if max_attempts < 1 then
    Error
      (Error.validation ~field:"max_attempts"
         "retry max_attempts must be at least 1")
  else if Ptime.Span.compare base_delay Ptime.Span.zero < 0 then
    Error
      (Error.validation ~field:"base_delay"
         "retry base_delay must be non-negative")
  else if Ptime.Span.compare max_delay Ptime.Span.zero < 0 then
    Error
      (Error.validation ~field:"max_delay"
         "retry max_delay must be non-negative")
  else if Ptime.Span.compare base_delay max_delay > 0 then
    Error
      (Error.validation ~field:"base_delay"
         "retry base_delay must be less than or equal to max_delay")
  else if Float.is_nan jitter || jitter < 0.0 || jitter > 1.0 then
    Error
      (Error.validation ~field:"jitter" "retry jitter must be between 0 and 1")
  else Ok { max_attempts; base_delay; max_delay; jitter }

let create_exn ?max_attempts ?base_delay ?max_delay ?jitter () =
  Error.get_ok_exn (create ?max_attempts ?base_delay ?max_delay ?jitter ())

let default = create_exn ()
let disabled = create_exn ~max_attempts:1 ()
let max_attempts t = t.max_attempts
let base_delay t = t.base_delay
let max_delay t = t.max_delay
let jitter t = t.jitter

let retryable error =
  match Error.retry_class error with
  | Retryable | Throttled -> true
  | Auth | Conflict | Not_found | Fatal | Unknown -> false

let capped_exponential_delay t ~attempt =
  let exponent = max 0 (attempt - 1) in
  let scale = 2. ** float_of_int exponent in
  let seconds =
    min
      (Ptime.Span.to_float_s t.max_delay)
      (Ptime.Span.to_float_s t.base_delay *. scale)
  in
  match Ptime.Span.of_float_s seconds with
  | Some delay -> delay
  | None -> t.max_delay

let delay ?random_float t ~attempt ~error =
  let base_delay =
    if attempt < 1 then None
    else if attempt >= t.max_attempts then None
    else if retryable error then Some (capped_exponential_delay t ~attempt)
    else None
  in
  match base_delay with
  | None -> None
  | Some delay when Float.equal t.jitter 0.0 -> Some delay
  | Some delay ->
      let random_float =
        Option.value
          ~default:(fun () -> Random.State.float (Random.get_state ()) 1.0)
          random_float
      in
      let factor = Float.max 0.0 (Float.min 1.0 (random_float ())) in
      let floor = 1.0 -. t.jitter in
      let seconds =
        Ptime.Span.to_float_s delay *. (floor +. (t.jitter *. factor))
      in
      Ptime.Span.of_float_s seconds
