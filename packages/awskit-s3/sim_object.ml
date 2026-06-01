open Core
open Sim_support

module Object = struct
  type connection = t
  type 'a io = 'a
  type request_body = Runtime.request_body
  type response_body_reader = Runtime.response_body_reader

  let invalid_range () = service ~status:416 ~code:"InvalidRange" ()

  let ranged_body body = function
    | None -> Ok (body, 200, [])
    | Some range -> (
        let length = String.length body in
        let length64 = Int64.of_int length in
        let bounds =
          match (range : Range.t) with
          | Bytes (start, finish) ->
              if Int64.compare start length64 >= 0 then None
              else
                let finish = Int64.min finish (Int64.sub length64 1L) in
                Some (start, finish)
          | From start ->
              if Int64.compare start length64 >= 0 then None
              else Some (start, Int64.sub length64 1L)
          | Suffix suffix ->
              if length = 0 then None
              else
                let start =
                  if Int64.compare suffix length64 >= 0 then 0L
                  else Int64.sub length64 suffix
                in
                Some (start, Int64.sub length64 1L)
        in
        match bounds with
        | None -> Error (invalid_range ())
        | Some (start, finish) ->
            let start_int = Int64.to_int start in
            let slice_length = Int64.(to_int (add (sub finish start) 1L)) in
            let headers =
              [
                ("content-range", Fmt.str "bytes %Ld-%Ld/%d" start finish length);
              ]
            in
            Ok (String.sub body start_int slice_length, 206, headers))

  let request_body_result body =
    let descriptor = Runtime.Request_body.descriptor body in
    match descriptor.content_length with
    | None ->
        Error
          (Awskit.Error.validation ~field:"content_length"
             "S3 uploads require a known content length before SigV4 chunked \
              streaming")
    | Some _ -> (
        match Awskit.Body.Request.validate_descriptor descriptor with
        | Error error -> Error error
        | Ok () -> Runtime.request_body_result body)

  let put conn ~bucket ~key ?options ~body () =
    let options = Option.value ~default:Put_object.default_options options in
    match validate_bucket_key bucket key with
    | Error error -> Error error
    | Ok () -> (
        match require_bucket conn bucket with
        | Error error -> Error error
        | Ok bucket_state -> (
            match validate_metadata options.metadata with
            | Error error -> Error error
            | Ok () -> (
                match validate_tags options.tags with
                | Error error -> Error error
                | Ok () -> (
                    match operation_fault conn `Put bucket (Some key) with
                    | Some error -> Error error
                    | None -> (
                        match
                          ensure_write_preconditions
                            (current_object bucket_state key)
                            options.preconditions
                        with
                        | Error error -> Error error
                        | Ok () -> (
                            match request_body_result body with
                            | Error error -> Error error
                            | Ok body ->
                                let etag = etag body in
                                let checksum =
                                  checksum_for_body ~body options.checksum
                                in
                                let obj =
                                  {
                                    body;
                                    etag;
                                    version_id = None;
                                    content_type = options.content_type;
                                    metadata = options.metadata;
                                    storage_class = options.storage_class;
                                    tags = options.tags;
                                    checksum;
                                    last_modified = now conn;
                                  }
                                in
                                let obj =
                                  store_object conn bucket_state key obj
                                in
                                Ok
                                  {
                                    Put_object.etag = Some etag;
                                    version_id = obj.version_id;
                                    checksum;
                                    response =
                                      response 200
                                        ~headers:
                                          (("etag", etag)
                                          :: (version_headers obj.version_id
                                             @ checksum_response_headers
                                                 checksum));
                                  }))))))

  let get conn ~bucket ~key ?options ~consume () =
    let options = Option.value ~default:Get_object.default_options options in
    match validate_bucket_key bucket key with
    | Error error -> Error error
    | Ok () -> (
        match require_bucket conn bucket with
        | Error error -> Error error
        | Ok bucket_state -> (
            match current_or_version bucket_state key options.version_id with
            | None -> Error (no_such_key ())
            | Some (Stored_delete_marker marker) ->
                Error
                  (delete_marker_error
                     ~current:(Option.is_none options.version_id)
                     marker)
            | Some (Stored_object obj) -> (
                match take_fault conn with
                | Some Response_lost -> (
                    record ~faulted:true conn `Get bucket (Some key);
                    match
                      ensure_read_preconditions obj options.preconditions
                    with
                    | Error error -> Error error
                    | Ok () ->
                        let* body, status, range_headers =
                          ranged_body obj.body options.range
                        in
                        let response =
                          response status
                            ~headers:
                              ([
                                 ("etag", obj.etag);
                                 ( "content-length",
                                   string_of_int (String.length body) );
                               ]
                              @ range_headers
                              @ version_headers obj.version_id
                              @ checksum_response_headers obj.checksum)
                        in
                        let info =
                          info_of_object ~content_length:(String.length body)
                            response obj
                        in
                        Result.map
                          (fun value -> (info, value))
                          (Runtime.Response_body.with_reader
                             (Runtime.response_body
                                ~read_fault:(fault_error Response_lost)
                                body)
                             ~consume))
                | Some fault ->
                    record ~faulted:true conn `Get bucket (Some key);
                    Error (fault_error fault)
                | None -> (
                    record conn `Get bucket (Some key);
                    match
                      ensure_read_preconditions obj options.preconditions
                    with
                    | Error error -> Error error
                    | Ok () ->
                        let* body, status, range_headers =
                          ranged_body obj.body options.range
                        in
                        let response =
                          response status
                            ~headers:
                              ([
                                 ("etag", obj.etag);
                                 ( "content-length",
                                   string_of_int (String.length body) );
                               ]
                              @ range_headers
                              @ version_headers obj.version_id
                              @ checksum_response_headers obj.checksum)
                        in
                        let info =
                          info_of_object ~content_length:(String.length body)
                            response obj
                        in
                        Result.map
                          (fun value -> (info, value))
                          (Runtime.Response_body.with_reader
                             (Runtime.response_body body)
                             ~consume)))))

  let head conn ~bucket ~key ?options () =
    let options = Option.value ~default:Head_object.default_options options in
    match validate_bucket_key bucket key with
    | Error error -> Error error
    | Ok () -> (
        match require_bucket conn bucket with
        | Error error -> Error error
        | Ok bucket_state -> (
            match current_or_version bucket_state key options.version_id with
            | None -> Error (no_such_key ())
            | Some (Stored_delete_marker marker) ->
                Error
                  (delete_marker_error
                     ~current:(Option.is_none options.version_id)
                     marker)
            | Some (Stored_object obj) -> (
                match operation_fault conn `Head bucket (Some key) with
                | Some error -> Error error
                | None -> (
                    match
                      ensure_read_preconditions obj options.preconditions
                    with
                    | Error error -> Error error
                    | Ok () ->
                        let response =
                          response 200
                            ~headers:
                              ([
                                 ("etag", obj.etag);
                                 ( "content-length",
                                   string_of_int (String.length obj.body) );
                               ]
                              @ version_headers obj.version_id
                              @ checksum_response_headers obj.checksum)
                        in
                        Ok (info_of_object response obj)))))

  let exists conn ~bucket ~key =
    match head conn ~bucket ~key () with
    | Ok _ -> Ok true
    | Error error when Error.is_not_found error -> Ok false
    | Error error -> Error error

  let delete_result ?delete_marker ?version_id () =
    {
      Delete_object.delete_marker;
      version_id;
      response =
        response 204
          ~headers:
            (version_headers version_id @ delete_marker_headers delete_marker);
    }

  let delete_objects_error key code message =
    { Delete_objects.key; code; message = Some message }

  let delete_objects_conditions_match object_ = function
    | Some (Stored_object obj) ->
        let etag_matches =
          match object_.Delete_objects.etag with
          | None -> true
          | Some etag -> Object.Etag.equal obj.etag etag
        in
        etag_matches
    | None | Some (Stored_delete_marker _) -> Option.is_none object_.etag

  let delete conn ~bucket ~key ?options () =
    let options = Option.value ~default:Delete_object.default_options options in
    match validate_bucket_key bucket key with
    | Error error -> Error error
    | Ok () -> (
        match require_bucket conn bucket with
        | Error error -> Error error
        | Ok bucket_state -> (
            match operation_fault conn `Delete bucket (Some key) with
            | Some error -> Error error
            | None -> (
                match options.version_id with
                | Some version_id -> (
                    match delete_version bucket_state key version_id with
                    | None
                      when delete_preconditions_are_empty options.preconditions
                      ->
                        Ok (delete_result ~version_id ())
                    | None -> Error (precondition_failed ())
                    | Some (Stored_delete_marker _) ->
                        if delete_preconditions_are_empty options.preconditions
                        then
                          Ok (delete_result ~delete_marker:true ~version_id ())
                        else Error (precondition_failed ())
                    | Some (Stored_object obj) -> (
                        match
                          ensure_delete_preconditions obj options.preconditions
                        with
                        | Error error -> Error error
                        | Ok () -> Ok (delete_result ~version_id ())))
                | None when versioning_keeps_history bucket_state -> (
                    match current_object bucket_state key with
                    | None
                      when delete_preconditions_are_empty options.preconditions
                      ->
                        let marker =
                          store_delete_marker conn bucket_state key
                        in
                        Ok
                          (delete_result ~delete_marker:true
                             ~version_id:marker.version_id ())
                    | None -> Error (precondition_failed ())
                    | Some obj -> (
                        match
                          ensure_delete_preconditions obj options.preconditions
                        with
                        | Error error -> Error error
                        | Ok () ->
                            let marker =
                              store_delete_marker conn bucket_state key
                            in
                            Ok
                              (delete_result ~delete_marker:true
                                 ~version_id:marker.version_id ())))
                | None -> (
                    match current_object bucket_state key with
                    | None
                      when delete_preconditions_are_empty options.preconditions
                      ->
                        Hashtbl.remove bucket_state.objects key;
                        Ok (delete_result ())
                    | None -> Error (precondition_failed ())
                    | Some obj -> (
                        match
                          ensure_delete_preconditions obj options.preconditions
                        with
                        | Error error -> Error error
                        | Ok () ->
                            Hashtbl.remove bucket_state.objects key;
                            Ok (delete_result ()))))))

  let delete_objects conn ~bucket ~objects =
    match validate_bucket bucket with
    | Error error -> Error error
    | Ok () -> (
        match require_bucket conn bucket with
        | Error error -> Error error
        | Ok bucket_state -> (
            match operation_fault conn `Delete_objects bucket None with
            | Some error -> Error error
            | None ->
                let deleted, errors =
                  List.fold_right
                    (fun (object_ : Delete_objects.object_) ->
                      fun (deleted, errors) ->
                       let target =
                         match object_.version_id with
                         | Some version_id ->
                             find_version bucket_state object_.key version_id
                         | None ->
                             Hashtbl.find_opt bucket_state.objects object_.key
                       in
                       if not (delete_objects_conditions_match object_ target)
                       then
                         ( deleted,
                           delete_objects_error object_.key "PreconditionFailed"
                             "delete preconditions did not match"
                           :: errors )
                       else
                         let delete_marker, version_id =
                           match object_.version_id with
                           | Some version_id -> (
                               match
                                 delete_version bucket_state object_.key
                                   version_id
                               with
                               | Some (Stored_delete_marker _) ->
                                   (Some true, Some version_id)
                               | Some (Stored_object _) | None ->
                                   (None, Some version_id))
                           | None when versioning_keeps_history bucket_state ->
                               let marker =
                                 store_delete_marker conn bucket_state
                                   object_.key
                               in
                               (Some true, Some marker.version_id)
                           | None ->
                               Hashtbl.remove bucket_state.objects object_.key;
                               (None, None)
                         in
                         ( {
                             Delete_objects.key = object_.key;
                             version_id;
                             delete_marker;
                           }
                           :: deleted,
                           errors ))
                    objects ([], [])
                in
                Ok { Delete_objects.deleted; errors; response = response 200 }))

  let copy conn ~src_bucket ~src_key ~dst_bucket ~dst_key ?options () =
    let options = Option.value ~default:Copy_object.default_options options in
    match
      require_object_version conn src_bucket src_key options.source_version_id
    with
    | Error error -> Error error
    | Ok src -> (
        match require_bucket conn dst_bucket with
        | Error error -> Error error
        | Ok dst_bucket_state -> (
            match operation_fault conn `Copy dst_bucket (Some dst_key) with
            | Some error -> Error error
            | None -> (
                match
                  ensure_copy_source_preconditions src
                    options.source_preconditions
                with
                | Error error -> Error error
                | Ok () ->
                    let metadata =
                      match options.metadata with
                      | Some (`Replace metadata) -> metadata
                      | _ -> src.metadata
                    in
                    let checksum =
                      match
                        checksum_for_body ~body:src.body options.checksum
                      with
                      | Some checksum -> Some checksum
                      | None -> src.checksum
                    in
                    let obj =
                      {
                        body = src.body;
                        etag = src.etag;
                        version_id = None;
                        content_type = src.content_type;
                        metadata;
                        storage_class =
                          (match options.storage_class with
                          | Some sc -> Some sc
                          | None -> src.storage_class);
                        tags =
                          (match options.tags with
                          | Some tags -> tags
                          | None -> src.tags);
                        checksum;
                        last_modified = now conn;
                      }
                    in
                    let obj = store_object conn dst_bucket_state dst_key obj in
                    Ok
                      {
                        Copy_object.etag = Some obj.etag;
                        last_modified = Some obj.last_modified;
                        version_id = obj.version_id;
                        copy_source_version_id = src.version_id;
                        response =
                          response 200
                            ~headers:
                              (version_headers obj.version_id
                              @ copy_source_version_headers src.version_id);
                      })))

  type version_entry =
    | Object_version of List_object_versions.object_version
    | Delete_marker of List_object_versions.delete_marker

  let version_entry_key = function
    | Object_version version -> version.key
    | Delete_marker marker -> marker.key

  let version_entry_id = function
    | Object_version version -> version.version_id
    | Delete_marker marker -> marker.version_id

  let version_entries_after_marker key_marker version_id_marker entries =
    match key_marker with
    | None -> entries
    | Some key_marker ->
        let rec drop = function
          | [] -> []
          | entry :: rest -> (
              let key = version_entry_key entry in
              match String.compare key key_marker with
              | value when value > 0 -> entry :: rest
              | value when value < 0 -> drop rest
              | _ -> (
                  match version_id_marker with
                  | None -> drop rest
                  | Some marker -> (
                      match version_entry_id entry with
                      | Some version_id
                        when Object.Version_id.equal version_id marker ->
                          rest
                      | _ -> drop rest)))
        in
        drop entries

  let version_entry_is_current bucket key version =
    match (Hashtbl.find_opt bucket.objects key, version) with
    | Some (Stored_object current), Stored_object obj -> current == obj
    | Some (Stored_delete_marker current), Stored_delete_marker marker ->
        Object.Version_id.equal current.version_id marker.version_id
    | _ -> false

  let version_entries bucket (options : List_object_versions.options) =
    let from_version key version =
      let is_latest = Some (version_entry_is_current bucket key version) in
      match version with
      | Stored_object obj ->
          Object_version
            {
              List_object_versions.key;
              version_id = obj.version_id;
              is_latest;
              last_modified = Some obj.last_modified;
              etag = Some obj.etag;
              size = Some (Int64.of_int (String.length obj.body));
              storage_class = obj.storage_class;
              owner = None;
            }
      | Stored_delete_marker marker ->
          Delete_marker
            {
              List_object_versions.key;
              version_id = Some marker.version_id;
              is_latest;
              last_modified = Some marker.last_modified;
              owner = None;
            }
    in
    let versioned =
      Hashtbl.to_seq bucket.versions
      |> Seq.flat_map (fun (key, versions) ->
          versions |> List.to_seq |> Seq.map (fun version -> (key, version)))
      |> List.of_seq
    in
    let unversioned =
      Hashtbl.to_seq bucket.objects
      |> Seq.filter_map (fun (key, version) ->
          if Hashtbl.mem bucket.versions key then None else Some (key, version))
      |> List.of_seq
    in
    versioned @ unversioned
    |> List.filter (fun (key, _) ->
        match options.List_object_versions.prefix with
        | None -> true
        | Some prefix -> is_prefix ~prefix key)
    |> List.sort (fun (left_key, left_version) (right_key, right_version) ->
        match String.compare left_key right_key with
        | 0 ->
            let left_current =
              version_entry_is_current bucket left_key left_version
            in
            let right_current =
              version_entry_is_current bucket right_key right_version
            in
            Bool.compare right_current left_current
        | value -> value)
    |> List.map (fun (key, version) -> from_version key version)
    |> version_entries_after_marker options.key_marker options.version_id_marker

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
            match operation_fault conn `List_versions bucket None with
            | Some error -> Error error
            | None ->
                let all = version_entries bucket_state options in
                let max_keys =
                  Option.value ~default:conn.store.config.max_list_keys
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
            match operation_fault conn `List bucket None with
            | Some error -> Error error
            | None ->
                let all =
                  Hashtbl.to_seq bucket_state.objects
                  |> Seq.filter_map (function
                    | key, Stored_object obj -> (
                        match options.prefix with
                        | None -> Some (key, obj)
                        | Some prefix ->
                            if is_prefix ~prefix key then Some (key, obj)
                            else None)
                    | _, Stored_delete_marker _ -> None)
                  |> List.of_seq
                  |> List.sort (fun (a, _) (b, _) -> String.compare a b)
                in
                let all =
                  match options.continuation_token with
                  | Some token ->
                      List.filter
                        (fun (key, _) -> String.compare key token > 0)
                        all
                  | None -> (
                      match options.start_after with
                      | None -> all
                      | Some start_after ->
                          List.filter
                            (fun (key, _) -> String.compare key start_after > 0)
                            all)
                in
                let max_keys =
                  Option.value ~default:conn.store.config.max_list_keys
                    options.max_keys
                in
                let selected =
                  all |> List.to_seq |> Seq.take max_keys |> List.of_seq
                in
                let is_truncated = List.length all > List.length selected in
                let next_continuation_token =
                  if not is_truncated then None
                  else
                    match List.rev selected with
                    | [] -> None
                    | (key, _) :: _ -> Some key
                in
                let objects =
                  List.map
                    (fun (key, (obj : stored_object)) ->
                      {
                        List_objects_v2.key;
                        size = Some (Int64.of_int (String.length obj.body));
                        etag = Some obj.etag;
                        last_modified = Some obj.last_modified;
                        storage_class = obj.storage_class;
                        owner = None;
                        checksums = Option.to_list obj.checksum;
                      })
                    selected
                in
                Ok
                  {
                    List_objects_v2.bucket = Some bucket;
                    prefix = options.prefix;
                    delimiter = options.delimiter;
                    objects;
                    common_prefixes = [];
                    key_count = Some (List.length objects);
                    is_truncated;
                    continuation_token = options.continuation_token;
                    next_continuation_token;
                    response = response 200;
                  }))

  let list_keys conn ~bucket ?options () =
    Result.map
      (fun (page : List_objects_v2.page) ->
        List.map
          (fun (o : List_objects_v2.object_summary) -> o.key)
          page.objects)
      (list conn ~bucket ?options ())

  module Paginator = struct
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

  module Versions = struct
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

  let consume_string ~max_bytes reader =
    let chunk = Bytes.create 8192 in
    let buffer = Buffer.create 128 in
    let rec loop total =
      match
        Runtime.Response_body.read reader chunk ~off:0 ~len:(Bytes.length chunk)
      with
      | Error error -> Error error
      | Ok 0 -> Ok (Buffer.contents buffer)
      | Ok n ->
          let total = Int64.add total (Int64.of_int n) in
          if Int64.compare total max_bytes > 0 then
            Error
              (Awskit.Error.body ~limit:max_bytes
                 "response body exceeded max_bytes")
          else begin
            Buffer.add_subbytes buffer chunk 0 n;
            loop total
          end
    in
    loop 0L

  let get_as_string conn ~bucket ~key ~max_bytes ?options () =
    get conn ~bucket ~key ?options ~consume:(consume_string ~max_bytes) ()

  let get_as_bytes conn ~bucket ~key ~max_bytes ?options () =
    Result.map
      (fun (info, body) -> (info, Bytes.of_string body))
      (get_as_string conn ~bucket ~key ~max_bytes ?options ())

  module Tagging = struct
    let get conn ~bucket ~key =
      match require_object conn bucket key with
      | Error error -> Error error
      | Ok obj -> Ok { Object.Tagging.tags = obj.tags; response = response 200 }

    let put conn ~bucket ~key tags =
      match require_object conn bucket key with
      | Error error -> Error error
      | Ok obj -> (
          match validate_tags tags with
          | Error error -> Error error
          | Ok () ->
              obj.tags <- tags;
              Ok (response 200))

    let delete conn ~bucket ~key =
      match require_object conn bucket key with
      | Error error -> Error error
      | Ok obj ->
          obj.tags <- [];
          Ok (response 204)
  end
end
