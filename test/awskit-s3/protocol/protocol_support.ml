open Awskit_s3

let test_time = Ptime.of_date_time ((2026, 4, 8), ((12, 0, 0), 0)) |> Option.get

let credentials =
  Awskit.Credentials.create_exn ~access_key_id:"AKID"
    ~secret_access_key:"SECRET" ()

let ok_or_fail label result =
  match result with
  | Ok value -> value
  | Error error -> Alcotest.failf "%s: %a" label Awskit.Error.pp error

let to_alcotest test = QCheck_alcotest.to_alcotest ~speed_level:`Quick test
let header name headers = List.assoc_opt name headers

let is_decode_error error =
  match Awskit_s3.Error.kind error with Decode _ -> true | _ -> false

let tag key value = Tag.create_exn ~key ~value
let tag_set tags = Tag.Set.of_list_exn tags
let content_type value = Content_type.of_string_exn value
let account_id value = Account_id.of_string_exn value
let bucket_name value = Bucket_name.of_string_exn value
let object_key value = Object_key.of_string_exn value
