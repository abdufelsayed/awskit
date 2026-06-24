open Simulator_support
open Simulator_headers
open Simulator_state
open Simulator_error
open Simulator_store
open Simulator_checksum
open Simulator_runtime
open Simulator_object_body
open Simulator_object_delete
open Simulator_object_listing
open Simulator_object_read
open Simulator_object_tagging
open Simulator_object_versions
module Bucket_name = Awskit_s3.Bucket_name
module Error = Awskit_s3.Error
module Object_model = Awskit_s3.Object

module Object = struct
  type connection = t
  type 'a io = 'a
  type request_body = Runtime.request_body
  type response_body_reader = Runtime.response_body_reader

  let put = Simulator_object_write.put
  let get = Simulator_object_read.get
  let find = Simulator_object_read.find
  let head = Simulator_object_read.head

  let find_metadata conn ~bucket ~key ?options () =
    match head conn ~bucket ~key ?options () with
    | Ok value -> Ok (Some value)
    | Error error when Error.is_no_such_key error -> Ok None
    | Error error -> Error error

  let exists = Simulator_object_read.exists
  let delete = Simulator_object_delete.delete
  let delete_objects = Simulator_object_delete.delete_objects
  let copy = Simulator_object_copy.copy

  let validate_list_max_keys = function
    | None -> Ok ()
    | Some value when value >= 1 && value <= 1000 -> Ok ()
    | Some _ -> invalid ~field:"max_keys" "max_keys must be between 1 and 1000"

  let list_versions conn ~bucket ?options () =
    let options =
      Option.value ~default:Object_model.Versions.default_options options
    in
    let return_error error =
      Error (with_operation `List_object_versions ~bucket error)
    in
    match validate_bucket bucket with
    | Error error -> return_error error
    | Ok () -> (
        match validate_list_max_keys options.max_keys with
        | Error error -> return_error error
        | Ok () -> (
            match require_bucket conn bucket with
            | Error error -> return_error error
            | Ok bucket_state -> (
                match
                  operation_fault conn `List_object_versions bucket None
                with
                | Some error -> return_error error
                | None ->
                    let all = listing_entries bucket_state options in
                    let max_keys =
                      Option.value ~default:(config (store conn)).max_list_keys
                        options.max_keys
                    in
                    let selected =
                      all |> List.to_seq |> Seq.take max_keys |> List.of_seq
                    in
                    let is_truncated = List.length all > List.length selected in
                    let next_key_marker, next_version_id_marker =
                      if not is_truncated then (None, None)
                      else
                        match List.rev selected with
                        | [] -> (None, None)
                        | entry :: _ ->
                            ( Some (listing_entry_key_marker entry),
                              listing_entry_id entry )
                    in
                    let versions, delete_markers, common_prefixes =
                      List.fold_right
                        (fun entry (versions, delete_markers, common_prefixes)
                           ->
                          match entry with
                          | Version_entry (Object_version version) ->
                              ( version :: versions,
                                delete_markers,
                                common_prefixes )
                          | Version_entry (Delete_marker marker) ->
                              ( versions,
                                marker :: delete_markers,
                                common_prefixes )
                          | Common_prefix prefix ->
                              ( versions,
                                delete_markers,
                                prefix :: common_prefixes ))
                        selected ([], [], [])
                    in
                    Ok
                      {
                        Object_model.Versions.bucket =
                          Some (Bucket_name.of_string_exn bucket);
                        prefix = options.prefix;
                        delimiter = options.delimiter;
                        versions;
                        delete_markers;
                        common_prefixes;
                        is_truncated;
                        key_marker = options.key_marker;
                        version_id_marker = options.version_id_marker;
                        next_key_marker;
                        next_version_id_marker;
                        response = response 200;
                      })))

  let list conn ~bucket ?options () =
    let options =
      Option.value ~default:Object_model.List.default_options options
    in
    let return_error error =
      Error (with_operation `List_objects_v2 ~bucket error)
    in
    match validate_bucket bucket with
    | Error error -> return_error error
    | Ok () -> (
        match validate_list_max_keys options.max_keys with
        | Error error -> return_error error
        | Ok () -> (
            match require_bucket conn bucket with
            | Error error -> return_error error
            | Ok bucket_state -> (
                match operation_fault conn `List_objects_v2 bucket None with
                | Some error -> return_error error
                | None ->
                    Ok
                      (Simulator_object_listing.page
                         ~default_max_keys:(config (store conn)).max_list_keys
                         ~bucket bucket_state options ~response:(response 200)))
            ))

  module List = struct
    type 'acc fold_step = Continue of 'acc | Stop of 'acc

    let validate_max_pages = function
      | None -> Ok ()
      | Some value when value > 0 -> Ok ()
      | Some _ ->
          invalid ~field:"max_pages" "max_pages must be greater than zero"

    let validate_required_max_pages value =
      if value > 0 then Ok ()
      else invalid ~field:"max_pages" "max_pages must be greater than zero"

    let max_pages_exceeded max_pages =
      Awskit.Error.Producer.validation ~field:"max_pages"
        (Fmt.str "ListObjectsV2 collection exceeded max_pages bound (%d)"
           max_pages)

    let options_for_page (base : Object_model.List.options) continuation_token =
      {
        base with
        Object_model.List.continuation_token;
        start_after =
          (match continuation_token with
          | None -> base.start_after
          | Some _ -> None);
      }

    let fold_pages_until conn ~bucket ?options ?max_pages ~init ~f () =
      match validate_max_pages max_pages with
      | Error error -> Error error
      | Ok () ->
          let base =
            Option.value ~default:Object_model.List.default_options options
          in
          let rec loop continuation_token page_count acc =
            let options = options_for_page base continuation_token in
            match list conn ~bucket ~options () with
            | Error error -> Error error
            | Ok page -> (
                match f acc page with
                | Error error -> Error error
                | Ok (Stop acc) -> Ok acc
                | Ok (Continue acc) -> (
                    let page_count = page_count + 1 in
                    if not page.is_truncated then Ok acc
                    else
                      match max_pages with
                      | Some max_pages when page_count >= max_pages -> Ok acc
                      | _ -> (
                          match page.next_continuation_token with
                          | Some token -> loop (Some token) page_count acc
                          | None ->
                              Error
                                (decode
                                   "truncated list response missing \
                                    NextContinuationToken"))))
          in
          loop base.continuation_token 0 init

    let fold_pages conn ~bucket ?options ?max_pages ~init ~f () =
      fold_pages_until conn ~bucket ?options ?max_pages ~init
        ~f:(fun acc page -> Result.map (fun acc -> Continue acc) (f acc page))
        ()

    let collect_pages conn ~bucket ?options ~max_pages ~init ~f () =
      let* () = validate_required_max_pages max_pages in
      let base =
        Option.value ~default:Object_model.List.default_options options
      in
      let rec loop continuation_token page_count acc =
        let options = options_for_page base continuation_token in
        match list conn ~bucket ~options () with
        | Error error -> Error error
        | Ok page -> (
            match f acc page with
            | Error error -> Error error
            | Ok acc -> (
                let page_count = page_count + 1 in
                if not page.is_truncated then Ok acc
                else if page_count >= max_pages then
                  Error (max_pages_exceeded max_pages)
                else
                  match page.next_continuation_token with
                  | Some token -> loop (Some token) page_count acc
                  | None ->
                      Error
                        (decode
                           "truncated list response missing \
                            NextContinuationToken")))
      in
      loop base.continuation_token 0 init

    let pages conn ~bucket ?options ~max_pages () =
      Result.map Stdlib.List.rev
        (collect_pages conn ~bucket ?options ~max_pages ~init:[]
           ~f:(fun pages page -> Ok (page :: pages))
           ())

    let objects conn ~bucket ?options ~max_pages () =
      Result.map Stdlib.List.rev
        (collect_pages conn ~bucket ?options ~max_pages ~init:[]
           ~f:(fun objects (page : Object_model.List.page) ->
             Ok (Stdlib.List.rev_append page.objects objects))
           ())

    let keys conn ~bucket ?options ~max_pages () =
      Result.map Stdlib.List.rev
        (collect_pages conn ~bucket ?options ~max_pages ~init:[]
           ~f:(fun keys (page : Object_model.List.page) ->
             let page_keys =
               Stdlib.List.map
                 (fun (object_ : Object_model.List.object_summary) ->
                   object_.key)
                 page.objects
             in
             Ok (Stdlib.List.rev_append page_keys keys))
           ())
  end

  module Versions = struct
    type 'acc fold_step = Continue of 'acc | Stop of 'acc

    let validate_max_pages = function
      | None -> Ok ()
      | Some value when value > 0 -> Ok ()
      | Some _ ->
          invalid ~field:"max_pages" "max_pages must be greater than zero"

    let validate_required_max_pages value =
      if value > 0 then Ok ()
      else invalid ~field:"max_pages" "max_pages must be greater than zero"

    let max_pages_exceeded max_pages =
      Awskit.Error.Producer.validation ~field:"max_pages"
        (Fmt.str "ListObjectVersions collection exceeded max_pages bound (%d)"
           max_pages)

    let options_for_page (base : Object_model.Versions.options)
        (page : Object_model.Versions.page) =
      {
        base with
        Object_model.Versions.key_marker = page.next_key_marker;
        version_id_marker = page.next_version_id_marker;
      }

    let fold_pages_until conn ~bucket ?options ?max_pages ~init ~f () =
      match validate_max_pages max_pages with
      | Error error -> Error error
      | Ok () ->
          let base =
            Option.value ~default:Object_model.Versions.default_options options
          in
          let rec loop options page_count acc =
            match list_versions conn ~bucket ~options () with
            | Error error -> Error error
            | Ok page -> (
                match f acc page with
                | Error error -> Error error
                | Ok (Stop acc) -> Ok acc
                | Ok (Continue acc) -> (
                    let page_count = page_count + 1 in
                    if not page.is_truncated then Ok acc
                    else
                      match max_pages with
                      | Some max_pages when page_count >= max_pages -> Ok acc
                      | _ -> (
                          match page.next_key_marker with
                          | Some _ ->
                              let options = options_for_page base page in
                              loop options page_count acc
                          | None ->
                              Error
                                (decode
                                   "truncated version listing response missing \
                                    NextKeyMarker"))))
          in
          loop base 0 init

    let fold_pages conn ~bucket ?options ?max_pages ~init ~f () =
      fold_pages_until conn ~bucket ?options ?max_pages ~init
        ~f:(fun acc page -> Result.map (fun acc -> Continue acc) (f acc page))
        ()

    let collect_pages conn ~bucket ?options ~max_pages ~init ~f () =
      let* () = validate_required_max_pages max_pages in
      let base =
        Option.value ~default:Object_model.Versions.default_options options
      in
      let rec loop options page_count acc =
        match list_versions conn ~bucket ~options () with
        | Error error -> Error error
        | Ok page -> (
            match f acc page with
            | Error error -> Error error
            | Ok acc -> (
                let page_count = page_count + 1 in
                if not page.is_truncated then Ok acc
                else if page_count >= max_pages then
                  Error (max_pages_exceeded max_pages)
                else
                  match page.next_key_marker with
                  | Some _ ->
                      let options = options_for_page base page in
                      loop options page_count acc
                  | None ->
                      Error
                        (decode
                           "truncated version listing response missing \
                            NextKeyMarker")))
      in
      loop base 0 init

    let pages conn ~bucket ?options ~max_pages () =
      Result.map Stdlib.List.rev
        (collect_pages conn ~bucket ?options ~max_pages ~init:[]
           ~f:(fun pages page -> Ok (page :: pages))
           ())

    let object_versions conn ~bucket ?options ~max_pages () =
      Result.map Stdlib.List.rev
        (collect_pages conn ~bucket ?options ~max_pages ~init:[]
           ~f:(fun versions (page : Object_model.Versions.page) ->
             Ok (Stdlib.List.rev_append page.versions versions))
           ())

    let delete_markers conn ~bucket ?options ~max_pages () =
      Result.map Stdlib.List.rev
        (collect_pages conn ~bucket ?options ~max_pages ~init:[]
           ~f:(fun markers (page : Object_model.Versions.page) ->
             Ok (Stdlib.List.rev_append page.delete_markers markers))
           ())
  end

  module Tagging = Simulator_object_tagging.Tagging
end
