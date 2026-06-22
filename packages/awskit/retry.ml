module Aws_error = Error

type t = {
  max_attempts : int;
  base_delay : Ptime.Span.t;
  max_delay : Ptime.Span.t;
  jitter : float;
  budget : budget;
}

and budget = {
  capacity : int;
  retry_cost : int;
  timeout_cost : int;
  success_credit : int;
}

type budget_state = { available : int }

let span_exn seconds =
  match Ptime.Span.of_float_s seconds with
  | Some span -> span
  | None -> invalid_arg "Awskit.Retry: invalid span"

let default_base_delay = span_exn 0.1
let default_max_delay = span_exn 2.0

let default_budget =
  { capacity = 500; retry_cost = 5; timeout_cost = 10; success_credit = 1 }

let positive_int ~field value =
  if value > 0 then Ok ()
  else
    Error
      (Aws_error.Internal.validation ~field
         (Printf.sprintf "%s must be strictly positive" field))

let budget ?(capacity = default_budget.capacity)
    ?(retry_cost = default_budget.retry_cost)
    ?(timeout_cost = default_budget.timeout_cost)
    ?(success_credit = default_budget.success_credit) () =
  match
    [
      positive_int ~field:"capacity" capacity;
      positive_int ~field:"retry_cost" retry_cost;
      positive_int ~field:"timeout_cost" timeout_cost;
      positive_int ~field:"success_credit" success_credit;
    ]
  with
  | [] -> assert false
  | results -> (
      match
        List.find_map
          (function Error error -> Some error | Ok () -> None)
          results
      with
      | Some error -> Error error
      | None -> Ok { capacity; retry_cost; timeout_cost; success_credit })

let budget_exn ?capacity ?retry_cost ?timeout_cost ?success_credit () =
  Aws_error.Internal.get_ok_exn
    (budget ?capacity ?retry_cost ?timeout_cost ?success_credit ())

let create ?(max_attempts = 3) ?(base_delay = default_base_delay)
    ?(max_delay = default_max_delay) ?(jitter = 1.0) ?(budget = default_budget)
    () =
  if max_attempts < 1 then
    Error
      (Aws_error.Internal.validation ~field:"max_attempts"
         "retry max_attempts must be at least 1")
  else if Ptime.Span.compare base_delay Ptime.Span.zero < 0 then
    Error
      (Aws_error.Internal.validation ~field:"base_delay"
         "retry base_delay must be non-negative")
  else if Ptime.Span.compare max_delay Ptime.Span.zero < 0 then
    Error
      (Aws_error.Internal.validation ~field:"max_delay"
         "retry max_delay must be non-negative")
  else if Ptime.Span.compare base_delay max_delay > 0 then
    Error
      (Aws_error.Internal.validation ~field:"base_delay"
         "retry base_delay must be less than or equal to max_delay")
  else if Float.is_nan jitter || jitter < 0.0 || jitter > 1.0 then
    Error
      (Aws_error.Internal.validation ~field:"jitter"
         "retry jitter must be between 0 and 1")
  else Ok { max_attempts; base_delay; max_delay; jitter; budget }

let create_exn ?max_attempts ?base_delay ?max_delay ?jitter ?budget () =
  Aws_error.Internal.get_ok_exn
    (create ?max_attempts ?base_delay ?max_delay ?jitter ?budget ())

let default = create_exn ()
let disabled = create_exn ~max_attempts:1 ()
let max_attempts t = t.max_attempts
let base_delay t = t.base_delay
let max_delay t = t.max_delay
let jitter t = t.jitter
let retry_budget t = t.budget
let budget_capacity budget = budget.capacity

let retry_cost t error =
  if Aws_error.is_timeout error then t.budget.timeout_cost
  else t.budget.retry_cost

let success_credit t = t.budget.success_credit
let initial_budget_state t = { available = t.budget.capacity }
let available_capacity t = t.available

let charge_retry t state error =
  let cost = retry_cost t error in
  if state.available < cost then None
  else Some { available = state.available - cost }

let credit_success t state =
  {
    available = min t.budget.capacity (state.available + t.budget.success_credit);
  }

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

let delay ~random_float t ~attempt ~error =
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
      let upper_bound = Ptime.Span.to_float_s delay in
      let random_delay =
        Float.max 0.0 (Float.min upper_bound (random_float ~upper_bound))
      in
      let deterministic_floor = upper_bound *. (1.0 -. t.jitter) in
      let seconds = deterministic_floor +. (t.jitter *. random_delay) in
      Ptime.Span.of_float_s seconds
