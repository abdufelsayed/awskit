open Awskit_s3_core
open Awskit_s3_sim_support

module Object = struct
  type connection = t
  type 'a io = 'a
  type upload_body = Runtime.upload_body
  type download_reader = Runtime.download_reader

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

  let put conn ~bucket ~key ?options ~body () =
    let options = Option.value ~default:Object.Put.default_options options in
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
                            (Hashtbl.find_opt bucket_state.objects key)
                            options.preconditions
                        with
                        | Error error -> Error error
                        | Ok () ->
                            let etag = etag body in
                            let checksum =
                              checksum_for_body ~body options.checksum
                            in
                            let obj =
                              {
                                body;
                                etag;
                                content_type = options.content_type;
                                metadata = options.metadata;
                                storage_class = options.storage_class;
                                tags = options.tags;
                                checksum;
                                last_modified = now conn;
                              }
                            in
                            Hashtbl.replace bucket_state.objects key obj;
                            Ok
                              {
                                Object.Put.etag = Some etag;
                                version_id = None;
                                checksum;
                                request =
                                  response 200
                                    ~headers:
                                      (("etag", etag)
                                      :: checksum_response_headers checksum);
                              })))))

  let get conn ~bucket ~key ?options ~consume () =
    let options = Option.value ~default:Object.Get.default_options options in
    match validate_bucket_key bucket key with
    | Error error -> Error error
    | Ok () -> (
        match require_object conn bucket key with
        | Error error -> Error error
        | Ok obj -> (
            match take_fault conn with
            | Some Response_lost -> (
                record ~faulted:true conn `Get bucket (Some key);
                match ensure_read_preconditions obj options.preconditions with
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
                          @ checksum_response_headers obj.checksum)
                    in
                    let info =
                      info_of_object ~content_length:(String.length body)
                        response obj
                    in
                    Result.map
                      (fun value -> (info, value))
                      (Runtime.with_download_body
                         (Runtime.download_body
                            ~read_fault:(fault_error Response_lost)
                            body)
                         ~consume))
            | Some fault ->
                record ~faulted:true conn `Get bucket (Some key);
                Error (fault_error fault)
            | None -> (
                record conn `Get bucket (Some key);
                match ensure_read_preconditions obj options.preconditions with
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
                          @ checksum_response_headers obj.checksum)
                    in
                    let info =
                      info_of_object ~content_length:(String.length body)
                        response obj
                    in
                    Result.map
                      (fun value -> (info, value))
                      (Runtime.with_download_body
                         (Runtime.download_body body)
                         ~consume))))

  let head conn ~bucket ~key ?options () =
    let options = Option.value ~default:Object.Head.default_options options in
    match validate_bucket_key bucket key with
    | Error error -> Error error
    | Ok () -> (
        match require_object conn bucket key with
        | Error error -> Error error
        | Ok obj -> (
            match operation_fault conn `Head bucket (Some key) with
            | Some error -> Error error
            | None -> (
                match ensure_read_preconditions obj options.preconditions with
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
                          @ checksum_response_headers obj.checksum)
                    in
                    Ok (info_of_object response obj))))

  let exists conn ~bucket ~key =
    match head conn ~bucket ~key () with
    | Ok _ -> Ok true
    | Error error when Error.is_not_found error -> Ok false
    | Error error -> Error error

  let delete conn ~bucket ~key ?options () =
    let options = Option.value ~default:Object.Delete.default_options options in
    match validate_bucket_key bucket key with
    | Error error -> Error error
    | Ok () -> (
        match require_bucket conn bucket with
        | Error error -> Error error
        | Ok bucket_state -> (
            match operation_fault conn `Delete bucket (Some key) with
            | Some error -> Error error
            | None -> (
                match Hashtbl.find_opt bucket_state.objects key with
                | None when delete_preconditions_are_empty options.preconditions
                  ->
                    Hashtbl.remove bucket_state.objects key;
                    Ok
                      {
                        Object.Delete.delete_marker = None;
                        version_id = None;
                        request = response 204;
                      }
                | None -> Error (precondition_failed ())
                | Some obj -> (
                    match
                      ensure_delete_preconditions obj options.preconditions
                    with
                    | Error error -> Error error
                    | Ok () ->
                        Hashtbl.remove bucket_state.objects key;
                        Ok
                          {
                            Object.Delete.delete_marker = None;
                            version_id = None;
                            request = response 204;
                          }))))

  let delete_many conn ~bucket ~objects =
    match validate_bucket bucket with
    | Error error -> Error error
    | Ok () -> (
        match require_bucket conn bucket with
        | Error error -> Error error
        | Ok bucket_state -> (
            match operation_fault conn `Delete_many bucket None with
            | Some error -> Error error
            | None ->
                let deleted =
                  List.map
                    (fun (object_ : Object.Delete_many.object_) ->
                      Hashtbl.remove bucket_state.objects object_.key;
                      {
                        Object.Delete_many.key = object_.key;
                        version_id = object_.version_id;
                        delete_marker = None;
                      })
                    objects
                in
                Ok
                  {
                    Object.Delete_many.deleted;
                    errors = [];
                    request = response 200;
                  }))

  let copy conn ~src_bucket ~src_key ~dst_bucket ~dst_key ?options () =
    let options = Option.value ~default:Object.Copy.default_options options in
    match require_object conn src_bucket src_key with
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
                    Hashtbl.replace dst_bucket_state.objects dst_key obj;
                    Ok
                      {
                        Object.Copy.etag = Some obj.etag;
                        last_modified = Some obj.last_modified;
                        version_id = None;
                        copy_source_version_id = None;
                        request = response 200;
                      })))

  let list conn ~bucket ?options () =
    let options = Option.value ~default:Object.List.default_options options in
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
                  |> Seq.filter (fun (key, _) ->
                      match options.prefix with
                      | None -> true
                      | Some prefix -> is_prefix ~prefix key)
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
                        Object.List.key;
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
                    Object.List.bucket = Some bucket;
                    prefix = options.prefix;
                    delimiter = options.delimiter;
                    objects;
                    common_prefixes = [];
                    key_count = Some (List.length objects);
                    is_truncated;
                    continuation_token = options.continuation_token;
                    next_continuation_token;
                    request = response 200;
                  }))

  let list_keys conn ~bucket ?options () =
    Result.map
      (fun (page : Object.List.page) ->
        List.map (fun (o : Object.List.object_summary) -> o.key) page.objects)
      (list conn ~bucket ?options ())

  module Paginator = struct
    let validate_max_pages = function
      | None -> Ok ()
      | Some value when value > 0 -> Ok ()
      | Some _ ->
          invalid ~field:"max_pages" "max_pages must be greater than zero"

    let options_for_page (base : Object.List.options) continuation_token =
      {
        base with
        Object.List.continuation_token;
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
            Option.value ~default:Object.List.default_options options
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
           ~f:(fun objects (page : Object.List.page) ->
             Ok (List.rev_append page.objects objects))
           ())

    let keys conn ~bucket ?options ?max_pages () =
      Result.map List.rev
        (fold_pages conn ~bucket ?options ?max_pages ~init:[]
           ~f:(fun keys (page : Object.List.page) ->
             let page_keys =
               List.map
                 (fun (object_ : Object.List.object_summary) -> object_.key)
                 page.objects
             in
             Ok (List.rev_append page_keys keys))
           ())
  end

  module Buffer = struct
    let put_string conn ~bucket ~key ?options body =
      put conn ~bucket ~key ?options ~body ()

    let put_bytes conn ~bucket ~key ?options body =
      put conn ~bucket ~key ?options ~body:(Bytes.to_string body) ()

    let consume_string ~max_size reader =
      let chunk = Bytes.create 8192 in
      let buffer = Buffer.create 128 in
      let rec loop total =
        match Runtime.read reader chunk ~off:0 ~len:(Bytes.length chunk) with
        | Error error -> Error error
        | Ok 0 -> Ok (Buffer.contents buffer)
        | Ok n ->
            let total = Int64.add total (Int64.of_int n) in
            if Int64.compare total max_size > 0 then
              Error
                (Awskit.Error.body ~limit:max_size
                   "download body exceeded max_size")
            else begin
              Buffer.add_subbytes buffer chunk 0 n;
              loop total
            end
      in
      loop 0L

    let get_string conn ~bucket ~key ~max_size ?options () =
      get conn ~bucket ~key ?options ~consume:(consume_string ~max_size) ()

    let get_bytes conn ~bucket ~key ~max_size ?options () =
      Result.map
        (fun (info, body) -> (info, Bytes.of_string body))
        (get_string conn ~bucket ~key ~max_size ?options ())
  end

  module Tagging = struct
    let get conn ~bucket ~key =
      match require_object conn bucket key with
      | Error error -> Error error
      | Ok obj -> Ok { Object.Tagging.tags = obj.tags; request = response 200 }

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
