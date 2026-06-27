type phase =
  [ `Connect | `Attempt | `Operation | `Request_body | `Response_body | `Drain ]

type policy = {
  connect : Ptime.Span.t option;
  attempt : Ptime.Span.t option;
  operation : Ptime.Span.t option;
  request_body : Ptime.Span.t option;
  response_body : Ptime.Span.t option;
  drain : Ptime.Span.t option;
}

let positive_span ~field = function
  | None -> Ok ()
  | Some span ->
      if Ptime.Span.to_float_s span > 0. then Ok ()
      else
        Error
          (Error.Producer.validation ~field
             "timeout span must be strictly positive")

let create ?connect ?attempt ?operation ?request_body ?response_body ?drain () =
  match
    [
      positive_span ~field:"connect" connect;
      positive_span ~field:"attempt" attempt;
      positive_span ~field:"operation" operation;
      positive_span ~field:"request_body" request_body;
      positive_span ~field:"response_body" response_body;
      positive_span ~field:"drain" drain;
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
      | None ->
          Ok { connect; attempt; operation; request_body; response_body; drain }
      )

let create_exn ?connect ?attempt ?operation ?request_body ?response_body ?drain
    () =
  match
    create ?connect ?attempt ?operation ?request_body ?response_body ?drain ()
  with
  | Ok policy -> policy
  | Error error -> raise (Error.Awskit_error error)

let span_of_seconds seconds =
  match Ptime.Span.of_float_s seconds with
  | Some span -> span
  | None -> invalid_arg "invalid timeout span"

let default =
  {
    connect = Some (span_of_seconds 3.1);
    attempt = None;
    operation = None;
    request_body = None;
    response_body = None;
    drain = Some (span_of_seconds 10.);
  }

let disabled =
  {
    connect = None;
    attempt = None;
    operation = None;
    request_body = None;
    response_body = None;
    drain = None;
  }

let span policy = function
  | `Connect -> policy.connect
  | `Attempt -> policy.attempt
  | `Operation -> policy.operation
  | `Request_body -> policy.request_body
  | `Response_body -> policy.response_body
  | `Drain -> policy.drain
