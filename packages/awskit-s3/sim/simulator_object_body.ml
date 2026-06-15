open Awskit_s3
open Simulator_support
open Simulator_error
open Simulator_runtime

let invalid_range () = service ~status:416 ~code:"InvalidRange" ()

let ranged_body body = function
  | None -> Ok (body, 200, [])
  | Some range -> (
      let length = String.length body in
      let length64 = Int64.of_int length in
      let bounds =
        match Range.view range with
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
        (Awskit.Error.Internal.validation ~field:"content_length"
           "S3 uploads require a known content length before SigV4 chunked \
            streaming")
  | Some _ -> (
      match Awskit.Body.Request.validate_descriptor descriptor with
      | Error error -> Error error
      | Ok () -> Runtime.request_body_result body)

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
            (Awskit.Error.Internal.body ~limit:max_bytes
               "response body exceeded max_bytes")
        else begin
          Buffer.add_subbytes buffer chunk 0 n;
          loop total
        end
  in
  loop 0L
