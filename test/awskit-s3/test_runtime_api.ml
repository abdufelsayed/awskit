module Minimal_runtime : Awskit_s3.RUNTIME = struct
  type 'a t = 'a
  type connection = unit
  type request_body = string
  type request_body_writer = Buffer.t
  type response_body = string
  type response_body_reader = string ref

  module IO = struct
    type 'a t = 'a

    let return x = x
    let bind x f = f x
  end

  module Request_body = struct
    type 'a io = 'a
    type t = request_body
    type writer = request_body_writer

    let empty = ""
    let of_string value = value
    let of_bytes value = Bytes.to_string value
    let of_stream _descriptor ~write:_ = ""

    let descriptor value =
      {
        Awskit.Body.Request.content_length =
          Some (Int64.of_int (String.length value));
        payload_hash = Awskit.Body.Payload_hash.sha256_of_string value;
        replayable = true;
      }

    let content_length value = Some (Int64.of_int (String.length value))

    let write_string writer value =
      Buffer.add_string writer value;
      Ok ()

    let write_bytes writer value =
      Buffer.add_bytes writer value;
      Ok ()

    let write_subbytes writer value ~off ~len =
      Buffer.add_subbytes writer value off len;
      Ok ()
  end

  module Response_body = struct
    type 'a io = 'a
    type t = response_body
    type reader = response_body_reader

    let read _ _ ~off:_ ~len:_ = Ok 0
    let next ?chunk_size:_ _ = Ok None
    let with_reader body ~consume = consume (ref body)
    let discard _ = Ok ()
  end

  module Transport = struct
    type 'a io = 'a
    type connection = unit
    type request_body = string
    type response_body = string

    let with_response _ _ ~body:_ ~consume =
      consume (Awskit.Response.create_exn ~status:200 ()) ""
  end

  module Clock = struct
    type connection = unit

    let now () = Ptime.epoch
  end

  module Sleeper = struct
    type 'a io = 'a
    type connection = unit

    let sleep () _ = ()
  end

  module Random = struct
    type connection = unit

    let float () ~upper_bound = upper_bound /. 2.
  end

  module Credentials = struct
    type 'a io = 'a
    type connection = unit

    let resolve () =
      Error (Awskit.Error.Producer.credentials "test credentials unavailable")
  end

  module Endpoint = struct
    type connection = unit

    let region () = Awskit.Region.of_string_exn "us-east-1"
    let endpoint () = None
  end

  module Retry = struct
    type connection = unit

    let policy () = Awskit.Retry.default
  end

  module Timeout = struct
    type connection = unit

    let policy () = Awskit.Timeout.default
  end

  module S3_endpoint = struct
    type connection = unit

    let s3_endpoint_config () = Awskit_s3.Endpoint_resolver.default
  end
end

module S3 = Awskit_s3.Make (Minimal_runtime)

let _ : S3.Body.t = S3.Body.of_string "body"
