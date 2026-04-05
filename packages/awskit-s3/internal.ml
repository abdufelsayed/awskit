(** Internal pure helpers shared by resource modules. Not part of public API. *)

open Base

let src = Logs.Src.create "aws-s3" ~doc:"S3"

module Log = (val Logs.src_log src : Logs.LOG)

module Let_syntax = struct
  let ( let* ) result f = Result.bind result ~f
  let ( let+ ) result f = Result.map result ~f
end

let resource_path bucket path query =
  let base = Fmt.str "/%s%s" bucket path in
  if String.is_empty query then base else base ^ "?" ^ query

let opt_header key value headers =
  match value with Some value -> (key, value) :: headers | None -> headers

(* ── XML helpers ────────────────────────────────────────────────── *)

module Xml = struct
  let root_of_string s =
    try
      let _, nodes = Ezxmlm.from_string s in
      match List.hd nodes with
      | Some node -> Ok node
      | None -> Error (`Invalid_xml "empty XML document")
    with exn -> Error (`Invalid_xml (Exn.to_string exn))

  let decode s ~name of_xml =
    match root_of_string s with
    | Error _ as error -> error
    | Ok xml -> (
        try Ok (of_xml xml)
        with exn ->
          Error (`Invalid_response (Fmt.str "%s: %s" name (Exn.to_string exn))))
end

(* ── Metadata header helpers ────────────────────────────────────── *)

module Metadata_headers = struct
  let prefix = "x-amz-meta-"

  let of_response_headers headers =
    List.filter_map headers ~f:(fun (key, value) ->
        let lower_key = String.lowercase key in
        if String.is_prefix lower_key ~prefix then
          Some (String.drop_prefix key (String.length prefix), value)
        else None)

  let to_headers (metadata : Metadata.t) =
    List.map metadata ~f:(fun (key, value) -> (prefix ^ key, value))
end

(* ── XML fragment helpers ───────────────────────────────────────── *)

let set_xml_name name = function
  | `El (((ns, _), attrs), elems) -> `El (((ns, name), attrs), elems)
  | fragment -> fragment
