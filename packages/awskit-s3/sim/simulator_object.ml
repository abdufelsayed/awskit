open Awskit_s3
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

  let list_versions conn ~bucket ?options () =
    let options =
      Option.value ~default:List_object_versions.default_options options
    in
    match validate_bucket bucket with
    | Error error -> Error error
    | Ok () -> (
        match require_bucket conn bucket with
        | Error error -> Error error
        | Ok bucket_state -> (
            match operation_fault conn `List_object_versions bucket None with
            | Some error -> Error error
            | None ->
                let all = version_entries bucket_state options in
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
                        (Some (version_entry_key entry), version_entry_id entry)
                in
                let versions, delete_markers =
                  List.fold_right
                    (fun entry (versions, delete_markers) ->
                      match entry with
                      | Object_version version ->
                          (version :: versions, delete_markers)
                      | Delete_marker marker ->
                          (versions, marker :: delete_markers))
                    selected ([], [])
                in
                Ok
                  {
                    List_object_versions.bucket = Some bucket;
                    prefix = options.prefix;
                    delimiter = options.delimiter;
                    versions;
                    delete_markers;
                    common_prefixes = [];
                    is_truncated;
                    key_marker = options.key_marker;
                    version_id_marker = options.version_id_marker;
                    next_key_marker;
                    next_version_id_marker;
                    response = response 200;
                  }))

  let list conn ~bucket ?options () =
    let options =
      Option.value ~default:List_objects_v2.default_options options
    in
    match validate_bucket bucket with
    | Error error -> Error error
    | Ok () -> (
        match require_bucket conn bucket with
        | Error error -> Error error
        | Ok bucket_state -> (
            match operation_fault conn `List_objects_v2 bucket None with
            | Some error -> Error error
            | None ->
                Ok
                  (Simulator_object_listing.page
                     ~default_max_keys:(config (store conn)).max_list_keys
                     ~bucket bucket_state options ~response:(response 200))))

  let list_keys conn ~bucket ?options () =
    Result.map
      (fun (page : List_objects_v2.page) ->
        List.map
          (fun (o : List_objects_v2.object_summary) -> o.key)
          page.objects)
      (list conn ~bucket ?options ())

  module List_objects_v2 = struct
    let validate_max_pages = function
      | None -> Ok ()
      | Some value when value > 0 -> Ok ()
      | Some _ ->
          invalid ~field:"max_pages" "max_pages must be greater than zero"

    let options_for_page (base : List_objects_v2.options) continuation_token =
      {
        base with
        List_objects_v2.continuation_token;
        start_after =
          (match continuation_token with
          | None -> base.start_after
          | Some _ -> None);
      }

    let fold_pages conn ~bucket ?options ?max_pages ~init ~f () =
      match validate_max_pages max_pages with
      | Error error -> Error error
      | Ok () ->
          let base =
            Option.value ~default:List_objects_v2.default_options options
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

    let pages conn ~bucket ?options ?max_pages () =
      Result.map List.rev
        (fold_pages conn ~bucket ?options ?max_pages ~init:[]
           ~f:(fun pages page -> Ok (page :: pages))
           ())

    let objects conn ~bucket ?options ?max_pages () =
      Result.map List.rev
        (fold_pages conn ~bucket ?options ?max_pages ~init:[]
           ~f:(fun objects (page : List_objects_v2.page) ->
             Ok (List.rev_append page.objects objects))
           ())

    let keys conn ~bucket ?options ?max_pages () =
      Result.map List.rev
        (fold_pages conn ~bucket ?options ?max_pages ~init:[]
           ~f:(fun keys (page : List_objects_v2.page) ->
             let page_keys =
               List.map
                 (fun (object_ : List_objects_v2.object_summary) -> object_.key)
                 page.objects
             in
             Ok (List.rev_append page_keys keys))
           ())
  end

  module List_object_versions = struct
    let validate_max_pages = function
      | None -> Ok ()
      | Some value when value > 0 -> Ok ()
      | Some _ ->
          invalid ~field:"max_pages" "max_pages must be greater than zero"

    let options_for_page (base : List_object_versions.options) page =
      {
        base with
        List_object_versions.key_marker =
          page.List_object_versions.next_key_marker;
        version_id_marker = page.next_version_id_marker;
      }

    let fold_pages conn ~bucket ?options ?max_pages ~init ~f () =
      match validate_max_pages max_pages with
      | Error error -> Error error
      | Ok () ->
          let base =
            Option.value ~default:List_object_versions.default_options options
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
                    else
                      match max_pages with
                      | Some max_pages when page_count >= max_pages -> Ok acc
                      | _ -> (
                          match page.next_key_marker with
                          | Some _ ->
                              loop (options_for_page base page) page_count acc
                          | None ->
                              Error
                                (decode
                                   "truncated version listing response missing \
                                    NextKeyMarker"))))
          in
          loop base 0 init

    let pages conn ~bucket ?options ?max_pages () =
      Result.map List.rev
        (fold_pages conn ~bucket ?options ?max_pages ~init:[]
           ~f:(fun pages page -> Ok (page :: pages))
           ())

    let object_versions conn ~bucket ?options ?max_pages () =
      Result.map List.rev
        (fold_pages conn ~bucket ?options ?max_pages ~init:[]
           ~f:(fun versions (page : List_object_versions.page) ->
             Ok (List.rev_append page.versions versions))
           ())

    let delete_markers conn ~bucket ?options ?max_pages () =
      Result.map List.rev
        (fold_pages conn ~bucket ?options ?max_pages ~init:[]
           ~f:(fun markers (page : List_object_versions.page) ->
             Ok (List.rev_append page.delete_markers markers))
           ())
  end

  let put_string conn ~bucket ~key ?options body =
    put conn ~bucket ~key ?options
      ~body:(Runtime.Request_body.of_string body)
      ()

  let put_bytes conn ~bucket ~key ?options body =
    put conn ~bucket ~key ?options ~body:(Runtime.Request_body.of_bytes body) ()

  let get_as_string = Simulator_object_read.get_as_string
  let get_as_bytes = Simulator_object_read.get_as_bytes

  module Tagging = Simulator_object_tagging.Tagging
end
