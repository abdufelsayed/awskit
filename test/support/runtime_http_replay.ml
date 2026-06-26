open Base
module Model = Runtime_http_model

type t = { path : string; scenario : Model.scenario }

let corpus_path name =
  Stdlib.Filename.concat "test"
    (Stdlib.Filename.concat "fixtures"
       (Stdlib.Filename.concat "runtime-http-replay" name))

let malformed_chunked_body_error =
  let path = corpus_path "malformed-chunked-body-error" in
  {
    path;
    scenario =
      Model.scenario ~name:path ~method_:`GET ~status:200
        ~framing:(Malformed_chunked "5\r\nhello\r\nnot-hex\r\n")
        ~connection:Close ();
  }

let all = [ malformed_chunked_body_error ]
