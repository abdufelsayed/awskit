type t = { aws : Awskit_lwt_unix.t; provider : Awskit_s3.Provider.t }

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
  let s3_provider t = t.provider

  let endpoint t =
    match Awskit_s3.Provider.endpoint t.provider ~region:(region t) with
    | Ok endpoint -> Some endpoint
    | Error _ -> None

  let empty_body = Awskit_lwt_unix.Runtime.empty_body
  let string_body = Awskit_lwt_unix.Runtime.string_body
  let bytes_body = Awskit_lwt_unix.Runtime.bytes_body
  let stream_body = Awskit_lwt_unix.Runtime.stream_body
  let upload_descriptor = Awskit_lwt_unix.Runtime.upload_descriptor
  let write_string = Awskit_lwt_unix.Runtime.write_string
  let read = Awskit_lwt_unix.Runtime.read
  let with_download_body = Awskit_lwt_unix.Runtime.with_download_body
  let call t request body = Awskit_lwt_unix.Runtime.call t.aws request body
end

module S3 = Awskit_s3.Make (Runtime)

let create ?ctx ?(provider = Awskit_s3.Provider.default) ?region ?credentials
    ?clock () =
  let region = Option.map Awskit.Region.to_string region in
  match Awskit_lwt_unix.create ?ctx ?region ?credentials ?clock () with
  | Error _ as error -> error
  | Ok aws -> Ok { aws; provider }

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
