module Credentials = struct
  module Provider = struct
    type credentials = Awskit.Credentials.t
    type t = unit -> (credentials, Awskit.Error.t) Result.t Lwt.t

    let create f = f
    let resolve t = t ()
    let static credentials = fun () -> Lwt.return_ok credentials

    let chain providers =
     fun () ->
      let rec loop errors = function
        | [] ->
            let errors = List.rev errors in
            let error =
              match errors with
              | [] ->
                  Awskit.Error.validation ~field:"credentials"
                    "no credential providers configured"
              | errors ->
                  Awskit.Error.multiple errors
                  |> Awskit.Error.with_context
                       "no credential provider resolved credentials"
            in
            Lwt.return_error error
        | provider :: rest ->
            Lwt.bind (provider ()) (function
              | Ok _ as ok -> Lwt.return ok
              | Error error -> loop (error :: errors) rest)
      in
      loop [] providers
  end
end

module Make = Runtime.Make
