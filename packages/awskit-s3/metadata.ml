let ( let* ) = S3_result.( let* )

type entry = { key : string; value : Header_value.t }
type t = { entries_rev : entry list; lower_keys : string list }

let prefix = "x-amz-meta-"

let invalid ?field fmt =
  Fmt.kstr
    (fun message -> Error (Awskit.Error.Producer.validation ?field message))
    fmt

let validate_key seen key =
  if key = "" then invalid ~field:"metadata" "metadata key must be non-empty"
  else if S3_string.has_ctl_or_del key then
    invalid ~field:"metadata" "metadata key contains control characters"
  else if S3_string.is_prefix ~prefix (String.lowercase_ascii key) then
    invalid ~field:"metadata" "metadata keys must not include x-amz-meta-"
  else
    let lower = String.lowercase_ascii key in
    if List.exists (String.equal lower) seen then
      invalid ~field:"metadata" "metadata key %S is duplicated" key
    else Ok lower

let empty = { entries_rev = []; lower_keys = [] }

let of_list entries =
  let rec loop seen acc = function
    | [] -> Ok { entries_rev = acc; lower_keys = seen }
    | (key, value) :: rest -> (
        match validate_key seen key with
        | Error _ as error -> error
        | Ok lower -> (
            match Header_value.of_string ~field:("metadata " ^ key) value with
            | Error _ as error -> error
            | Ok value -> loop (lower :: seen) ({ key; value } :: acc) rest))
  in
  loop [] [] entries

let of_list_exn entries = Awskit.Error.Producer.get_ok_exn (of_list entries)

let to_list metadata =
  List.rev_map
    (fun { key; value } -> (key, Header_value.to_string value))
    metadata.entries_rev

let entries metadata = List.rev metadata.entries_rev

let add ~key ~value metadata =
  let* lower = validate_key metadata.lower_keys key in
  let* value = Header_value.of_string ~field:("metadata " ^ key) value in
  Ok
    {
      entries_rev = { key; value } :: metadata.entries_rev;
      lower_keys = lower :: metadata.lower_keys;
    }

let pp fmt metadata =
  Fmt.Dump.list
    (Fmt.Dump.pair Fmt.Dump.string Fmt.Dump.string)
    fmt (to_list metadata)

let equal_entry left right =
  String.equal left.key right.key && Header_value.equal left.value right.value

let equal left right = List.equal equal_entry (entries left) (entries right)
