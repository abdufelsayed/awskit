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
            let message =
              match List.rev errors with
              | [] -> "no credential providers configured"
              | errors ->
                  "no credential provider resolved credentials: "
                  ^ String.concat "; "
                      (List.map Awskit.Error.to_string_hum errors)
            in
            Lwt.return_error
              (Awskit.Error.validation ~field:"credentials" message)
        | provider :: rest ->
            Lwt.bind (provider ()) (function
              | Ok _ as ok -> Lwt.return ok
              | Error error -> loop (error :: errors) rest)
      in
      loop [] providers
  end
end

module Make = Runtime.Make
