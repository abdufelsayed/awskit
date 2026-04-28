type t = {
  aws : Awskit_lwt_unix.t;
  endpoint_config : Awskit_s3.endpoint_config;
}

module Runtime = struct
  type connection = t
  type 'a t = 'a Lwt.t
  type upload_body = Awskit_lwt_unix.Runtime.upload_body
  type download_body = Awskit_lwt_unix.Runtime.download_body
  type upload_writer = Awskit_lwt_unix.Runtime.upload_writer
  type download_reader = Awskit_lwt_unix.Runtime.download_reader

  let return = Awskit_lwt_unix.Runtime.return
  let bind = Awskit_lwt_unix.Runtime.bind
  let now t = Awskit_lwt_unix.Runtime.now t.aws
  let region t = Awskit_lwt_unix.Runtime.region t.aws
  let credentials t = Awskit_lwt_unix.Runtime.credentials t.aws
  let retry_policy t = Awskit_lwt_unix.Runtime.retry_policy t.aws
  let sleep t = Awskit_lwt_unix.Runtime.sleep t.aws
  let s3_endpoint_config t = t.endpoint_config
  let endpoint _ = None
  let empty_body = Awskit_lwt_unix.Runtime.empty_body
  let string_body = Awskit_lwt_unix.Runtime.string_body
  let bytes_body = Awskit_lwt_unix.Runtime.bytes_body
  let stream_body = Awskit_lwt_unix.Runtime.stream_body
  let upload_descriptor = Awskit_lwt_unix.Runtime.upload_descriptor
  let write_string = Awskit_lwt_unix.Runtime.write_string
  let read = Awskit_lwt_unix.Runtime.read
  let with_download_body = Awskit_lwt_unix.Runtime.with_download_body
  let discard_download_body = Awskit_lwt_unix.Runtime.discard_download_body
  let call t request body = Awskit_lwt_unix.Runtime.call t.aws request body
end

module S3 = Awskit_s3.Make (Runtime)

let create ?ctx ?endpoint ?addressing_style ?endpoint_variant ?scheme ?region
    ?credentials ?clock ?retry_policy () =
  let region = Option.map Awskit.Region.to_string region in
  match
    Awskit_lwt_unix.create ?ctx ?region ?credentials ?clock ?retry_policy ()
  with
  | Error _ as error -> error
  | Ok aws ->
      let endpoint_config =
        Awskit_s3.endpoint_config ?addressing_style ?endpoint_variant ?scheme
          ?endpoint ()
      in
      Ok { aws; endpoint_config }

module Object = struct
  include S3.Object

  module Transfer = struct
    let upload_from_path conn ~bucket ~key ?options ~path () =
      Lwt.catch
        (fun () ->
          Lwt.bind
            (Lwt_io.with_file ~mode:Lwt_io.Input path (fun channel ->
                 Lwt_io.read channel))
            (fun body -> Buffer.put_string conn ~bucket ~key ?options body))
        (fun exn ->
          Lwt.return_error
            (Awskit.Error.body
               (Fmt.str "failed to read upload path %S: %s" path
                  (Printexc.to_string exn))))

    let download_to_path conn ~bucket ~key ?options ~path () =
      Lwt.bind
        (Buffer.get_string conn ~bucket ~key ~max_size:Int64.max_int ?options ())
        (function
        | Error _ as error -> Lwt.return error
        | Ok (info, body) ->
            Lwt.catch
              (fun () ->
                Lwt_io.with_file ~mode:Lwt_io.Output path (fun channel ->
                    Lwt_io.write channel body)
                |> Lwt.map (fun () -> Ok info))
              (fun exn ->
                Lwt.return_error
                  (Awskit.Error.body
                     (Fmt.str "failed to write download path %S: %s" path
                        (Printexc.to_string exn)))))
  end
end

module Bucket = S3.Bucket
module Multipart = S3.Multipart
module Presigned = S3.Presigned
