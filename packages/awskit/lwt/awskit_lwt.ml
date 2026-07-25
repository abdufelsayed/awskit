module Credentials = struct
  module Provider = struct
    type credentials = Awskit.Credentials.t
    type source = Awskit.Credentials.Provider.source
    type unavailable = { source : source; reason : string }

    type resolution =
      | Resolved of credentials
      | Unavailable of unavailable
      | Invalid of Awskit.Error.t
      | Failed of Awskit.Error.t

    type t = unit -> resolution Lwt.t

    let create f = f
    let resolve t = t ()

    let static credentials =
      let provider = Awskit.Credentials.Provider.static credentials in
      fun () ->
        match Awskit.Credentials.Provider.resolve provider with
        | Awskit.Credentials.Provider.Resolved credentials ->
            Lwt.return (Resolved credentials)
        | Awskit.Credentials.Provider.Unavailable { source; reason } ->
            Lwt.return (Unavailable { source; reason })
        | Awskit.Credentials.Provider.Invalid error ->
            Lwt.return (Invalid error)
        | Awskit.Credentials.Provider.Failed error -> Lwt.return (Failed error)

    let chain providers =
     fun () ->
      let rec loop last_unavailable = function
        | [] ->
            Lwt.return
              (Unavailable
                 (Option.value last_unavailable
                    ~default:
                      {
                        source = `Custom "chain";
                        reason = "no credential providers configured";
                      }))
        | provider :: rest ->
            Lwt.bind (provider ()) (function
              | Resolved _ as resolved -> Lwt.return resolved
              | Unavailable unavailable -> loop (Some unavailable) rest
              | Invalid _ as invalid -> Lwt.return invalid
              | Failed _ as failed -> Lwt.return failed)
      in
      loop None providers

    let source_label = Awskit.Credentials.Provider.source_label
  end
end

module Observability = Observer

let provider_resolution_to_result resolution =
  let open Credentials.Provider in
  match resolution with
  | Resolved credentials -> Ok credentials
  | Unavailable { source; reason } ->
      Error
        (Awskit.Error.Producer.credentials
           ~source:(Awskit.Credentials.Provider.source_label source)
           reason)
  | Invalid error | Failed error -> Error error

module For_connector = struct
  module type S = Runtime.CONNECTOR

  module Make (Connector : S) = struct
    module Inner = Runtime.Make_with_connector (Connector)

    type t = Inner.t

    module Runtime = Inner.Runtime
    module Runtime_observer = Inner.Runtime.Observability

    let create = Inner.create

    let create_with_credentials_provider ?ctx ?endpoint ~region
        ~credentials_provider ~clock ?retry_policy ?sleep ?random_float
        ?timeout_policy ?max_response_drain_bytes ?observability () =
      let credentials_provider () =
        Lwt.map provider_resolution_to_result
          (Credentials.Provider.resolve credentials_provider)
      in
      Inner.create_with_credentials_provider ?ctx ?endpoint ~region
        ~credentials_provider ~clock ?retry_policy ?sleep ?random_float
        ?timeout_policy ?max_response_drain_bytes ?observability ()
  end
end

module Make (Client : Cohttp_lwt.S.Client) = struct
  module Inner = Runtime.Make (Client)

  type t = Inner.t

  module Runtime = Inner.Runtime
  module Runtime_observer = Inner.Runtime.Observability

  let create = Inner.create

  let create_with_credentials_provider ?ctx ?endpoint ~region
      ~credentials_provider ~clock ?retry_policy ?sleep ?random_float
      ?timeout_policy ?max_response_drain_bytes ?observability () =
    let credentials_provider () =
      Lwt.map provider_resolution_to_result
        (Credentials.Provider.resolve credentials_provider)
    in
    Inner.create_with_credentials_provider ?ctx ?endpoint ~region
      ~credentials_provider ~clock ?retry_policy ?sleep ?random_float
      ?timeout_policy ?max_response_drain_bytes ?observability ()
end
