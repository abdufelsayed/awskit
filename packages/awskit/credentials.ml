open Base

type t = {
  access_key_id : string;
  secret_access_key : string;
  session_token : string option;
}

let make ~access_key_id ~secret_access_key ?session_token () =
  { access_key_id; secret_access_key; session_token }

let access_key_id t = t.access_key_id
let secret_access_key t = t.secret_access_key
let session_token t = t.session_token
