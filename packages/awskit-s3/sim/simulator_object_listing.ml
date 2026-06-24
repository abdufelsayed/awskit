module Bucket_name = Awskit_s3.Bucket_name
module Object = Awskit_s3.Object
module Object_key = Awskit_s3.Object_key

type listing_entry =
  | Object of string * Simulator_state.stored_object
  | Common_prefix of string

type page_builder = {
  seen_common_prefixes : string list;
  selected_rev : listing_entry list;
  selected_count : int;
  is_truncated : bool;
  marker : string option;
  max_keys : int;
}

type page_step = Continue of page_builder | Stop of page_builder

let continuation_token_prefix = "sim-v1-hex:"

let entry_marker = function
  | Object (key, _) -> key
  | Common_prefix prefix -> prefix

let hex_digit value =
  Char.chr
    (if value < 10 then Char.code '0' + value else Char.code 'a' + value - 10)

let hex_value = function
  | '0' .. '9' as value -> Some (Char.code value - Char.code '0')
  | 'a' .. 'f' as value -> Some (Char.code value - Char.code 'a' + 10)
  | 'A' .. 'F' as value -> Some (Char.code value - Char.code 'A' + 10)
  | _ -> None

let encode_marker marker =
  let encoded = Bytes.create (String.length marker * 2) in
  String.iteri
    (fun index char ->
      let byte = Char.code char in
      Bytes.set encoded (index * 2) (hex_digit (byte lsr 4));
      Bytes.set encoded ((index * 2) + 1) (hex_digit (byte land 0x0f)))
    marker;
  Bytes.unsafe_to_string encoded

let decode_marker encoded =
  let len = String.length encoded in
  if len mod 2 <> 0 then None
  else
    let decoded = Bytes.create (len / 2) in
    let rec loop index =
      if index = len then Some (Bytes.unsafe_to_string decoded)
      else
        match (hex_value encoded.[index], hex_value encoded.[index + 1]) with
        | Some high, Some low ->
            Bytes.set decoded (index / 2) (Char.chr ((high lsl 4) lor low));
            loop (index + 2)
        | _ -> None
    in
    loop 0

let encode_continuation_token marker =
  Object.List.Continuation_token.of_string_exn
    (continuation_token_prefix ^ encode_marker marker)

let decode_continuation_token token =
  let value = Object.List.Continuation_token.to_string token in
  if Simulator_support.is_prefix ~prefix:continuation_token_prefix value then
    let encoded =
      String.sub value
        (String.length continuation_token_prefix)
        (String.length value - String.length continuation_token_prefix)
    in
    match decode_marker encoded with Some marker -> marker | None -> value
  else value

let find_sub ~sub value =
  let sub_len = String.length sub in
  let value_len = String.length value in
  let rec loop index =
    if sub_len = 0 || index + sub_len > value_len then None
    else if String.equal (String.sub value index sub_len) sub then Some index
    else loop (index + 1)
  in
  loop 0

let visible_objects (bucket_state : Simulator_state.bucket_state)
    (options : Object.List.options) =
  let prefix = Option.map Object_key.Prefix.to_string options.prefix in
  Hashtbl.to_seq bucket_state.objects
  |> Seq.filter_map (function
    | key, Simulator_state.Stored_object obj -> (
        match prefix with
        | None -> Some (key, obj)
        | Some prefix ->
            if Simulator_support.is_prefix ~prefix key then Some (key, obj)
            else None)
    | _, Simulator_state.Stored_delete_marker _ -> None)
  |> List.of_seq
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let common_prefix_for_key (options : Object.List.options) key =
  match options.delimiter with
  | None -> None
  | Some delimiter -> (
      let prefix =
        Option.value ~default:""
          (Option.map Object_key.Prefix.to_string options.prefix)
      in
      let delimiter = Object.List.Delimiter.to_string delimiter in
      let rest =
        String.sub key (String.length prefix)
          (String.length key - String.length prefix)
      in
      match find_sub ~sub:delimiter rest with
      | None -> None
      | Some index ->
          let prefix_len = index + String.length delimiter in
          Some (prefix ^ String.sub rest 0 prefix_len))

let entry_of_object options (key, obj) =
  match common_prefix_for_key options key with
  | Some prefix -> Common_prefix prefix
  | None -> Object (key, obj)

let marker_from_options (options : Object.List.options) =
  match options.continuation_token with
  | Some token -> Some (decode_continuation_token token)
  | None -> Option.map Object_key.to_string options.start_after

let entry_after_marker marker entry =
  match marker with
  | None -> true
  | Some marker -> String.compare (entry_marker entry) marker > 0

let select_entry state entry =
  if not (entry_after_marker state.marker entry) then Continue state
  else if state.selected_count >= state.max_keys then
    Stop { state with is_truncated = true }
  else
    Continue
      {
        state with
        selected_rev = entry :: state.selected_rev;
        selected_count = state.selected_count + 1;
      }

let common_prefix_seen prefix state =
  List.exists (String.equal prefix) state.seen_common_prefixes

let add_entry state entry =
  match entry with
  | Common_prefix prefix when common_prefix_seen prefix state -> Continue state
  | Common_prefix prefix ->
      select_entry
        {
          state with
          seen_common_prefixes = prefix :: state.seen_common_prefixes;
        }
        entry
  | Object _ -> select_entry state entry

let page_entries bucket_state options ~max_keys =
  let state =
    {
      seen_common_prefixes = [];
      selected_rev = [];
      selected_count = 0;
      is_truncated = false;
      marker = marker_from_options options;
      max_keys;
    }
  in
  let rec loop state = function
    | [] -> state
    | object_ :: rest -> (
        match add_entry state (entry_of_object options object_) with
        | Stop state -> state
        | Continue state -> loop state rest)
  in
  let state = loop state (visible_objects bucket_state options) in
  (List.rev state.selected_rev, state.selected_count, state.is_truncated)

let page ~default_max_keys ~bucket bucket_state (options : Object.List.options)
    ~response =
  let max_keys = Option.value ~default:default_max_keys options.max_keys in
  let selected, selected_count, is_truncated =
    page_entries bucket_state options ~max_keys
  in
  let next_continuation_token =
    if not is_truncated then None
    else
      match List.rev selected with
      | [] -> None
      | entry :: _ -> Some (encode_continuation_token (entry_marker entry))
  in
  let objects =
    List.filter_map
      (function
        | Common_prefix _ -> None
        | Object (key, (obj : Simulator_state.stored_object)) ->
            Some
              {
                Object.List.key = Object_key.of_string_exn key;
                size = Some (Int64.of_int (String.length obj.body));
                etag = Some obj.etag;
                last_modified = Some obj.last_modified;
                storage_class = obj.storage_class;
                checksum = Simulator_checksum.checksum_summary obj.checksum;
              })
      selected
  in
  let common_prefixes =
    List.filter_map
      (function
        | Object _ -> None
        | Common_prefix prefix -> Some (Object_key.Prefix.of_string_exn prefix))
      selected
  in
  {
    Object.List.bucket = Some (Bucket_name.of_string_exn bucket);
    prefix = options.prefix;
    delimiter = options.delimiter;
    objects;
    common_prefixes;
    key_count = Some selected_count;
    is_truncated;
    continuation_token = options.continuation_token;
    next_continuation_token;
    response;
  }
