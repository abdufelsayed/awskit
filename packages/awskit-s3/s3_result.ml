let ( let* ) result f =
  match result with Ok value -> f value | Error _ as error -> error

let result_exn = Awskit.Error.Producer.get_ok_exn

let option_map f = function
  | None -> Ok None
  | Some value -> Result.map Option.some (f value)

let option_bind value f =
  match value with None -> None | Some value -> f value
