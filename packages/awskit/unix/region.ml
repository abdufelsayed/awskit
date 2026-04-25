open Base

module Env = struct
  type 'a t = ('a, Awskit.Error.base) Result.t

  let getenv_opt name = Stdlib.Sys.getenv_opt name

  let optional name =
    match getenv_opt name with
    | None -> Ok None
    | Some value when String.is_empty value ->
        Error (`Invalid_request (Fmt.str "%s is empty" name))
    | Some value -> Ok (Some value)
end

let from_env () =
  match Env.optional "AWS_REGION" with
  | Error _ as error -> error
  | Ok (Some region) -> Awskit.Region.of_string region
  | Ok None -> (
      match Env.optional "AWS_DEFAULT_REGION" with
      | Error _ as error -> error
      | Ok (Some region) -> Awskit.Region.of_string region
      | Ok None ->
          Error
            (`Invalid_request
               "AWS region not set; configure AWS_REGION or AWS_DEFAULT_REGION")
      )
