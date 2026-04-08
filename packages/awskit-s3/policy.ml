module S3_error = Error
open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Decode = struct
  type 'a t = ('a, S3_error.t) Result.t

  module Let_syntax = struct
    let ( let* ) result f = Result.bind result ~f
    let ( let+ ) result f = Result.map result ~f
  end

  let invalid_json message : 'a t = Error (`Invalid_json message)

  let all results =
    List.fold_right results ~init:(Ok []) ~f:(fun result acc ->
        match (result, acc) with
        | Ok value, Ok values -> Ok (value :: values)
        | (Error _ as error), _ -> error
        | _, (Error _ as error) -> error)

  let string_of_json ~context = function
    | `String value -> Ok value
    | _ -> invalid_json (Fmt.str "%s must be a string" context)

  let field fields name =
    match List.Assoc.find fields name ~equal:String.equal with
    | Some value -> Ok value
    | None -> invalid_json (Fmt.str "missing field %S" name)

  let field_opt fields name = List.Assoc.find fields name ~equal:String.equal

  let one_or_many_of_json ~context:_ decode = function
    | `List values -> List.map values ~f:decode |> all
    | value ->
        let open Let_syntax in
        let+ decoded = decode value in
        [ decoded ]

  let json_of_string s =
    try Ok (Yojson.Safe.from_string s)
    with exn -> invalid_json (Exn.to_string exn)
end

module Effect = struct
  type t = Allow | Deny [@@deriving show, eq]

  let to_string = function Allow -> "Allow" | Deny -> "Deny"

  let of_string = function
    | "Allow" -> Some Allow
    | "Deny" -> Some Deny
    | _ -> None

  let of_json json =
    let open Decode.Let_syntax in
    let* value = Decode.string_of_json ~context:"Effect" json in
    match of_string value with
    | Some effect_ -> Ok effect_
    | None -> Decode.invalid_json (Fmt.str "invalid Effect %S" value)

  let yojson_of_t effect_ = `String (to_string effect_)
end

module Principal = struct
  type t = Any | Aws of string list | User of string list
  [@@deriving show, eq]

  let matches ~principal ~access_key_id =
    match principal with
    | Any -> true
    | Aws arns -> List.exists arns ~f:(String.equal access_key_id)
    | User ids -> List.exists ids ~f:(String.equal access_key_id)

  let yojson_of_t = function
    | Any -> `String "*"
    | Aws arns ->
        `Assoc [ ("AWS", `List (List.map arns ~f:(fun arn -> `String arn))) ]
    | User ids ->
        `Assoc [ ("AWS", `List (List.map ids ~f:(fun id -> `String id))) ]

  let string_list_of_json ~context = function
    | `String value -> Ok [ value ]
    | `List values ->
        List.map values ~f:(Decode.string_of_json ~context) |> Decode.all
    | _ ->
        Decode.invalid_json
          (Fmt.str "%s must be a string or list of strings" context)

  let of_json = function
    | `String "*" -> Ok Any
    | `Assoc fields when List.length fields = 1 ->
        let open Decode.Let_syntax in
        let* value = Decode.field fields "AWS" in
        let+ ids = string_list_of_json ~context:"Principal.AWS" value in
        Aws ids
    | _ -> Decode.invalid_json "invalid Principal"
end

type target = Bucket_target of string | Object_target of string * string

module Resource = struct
  type t = Bucket of string | Bucket_objects of string [@@deriving show, eq]

  let to_arn = function
    | Bucket name -> "arn:aws:s3:::" ^ name
    | Bucket_objects name -> "arn:aws:s3:::" ^ name ^ "/*"

  let of_arn s =
    let prefix = "arn:aws:s3:::" in
    match String.chop_prefix s ~prefix with
    | None -> None
    | Some "" -> None
    | Some rest -> (
        match String.chop_suffix rest ~suffix:"/*" with
        | Some "" -> None
        | Some bucket -> Some (Bucket_objects bucket)
        | None -> Some (Bucket rest))

  let of_json json =
    let open Decode.Let_syntax in
    let* arn = Decode.string_of_json ~context:"Resource" json in
    match of_arn arn with
    | Some resource -> Ok resource
    | None -> Decode.invalid_json (Fmt.str "invalid Resource %S" arn)

  let matches ~resource ~target =
    match (resource, target) with
    | Bucket name, Bucket_target bucket -> String.equal name bucket
    | Bucket_objects name, Object_target (bucket, _key) ->
        String.equal name bucket
    | Bucket _, Object_target _ -> false
    | Bucket_objects _, Bucket_target _ -> false

  let yojson_of_t resource = `String (to_arn resource)

  type one_or_many = t list [@@deriving show, eq]

  let yojson_of_one_or_many resources =
    `List (List.map resources ~f:yojson_of_t)

  let one_or_many_of_json json =
    Decode.one_or_many_of_json ~context:"Resource" of_json json
end

module Action = struct
  type t = string [@@deriving show, eq]

  let of_json json = Decode.string_of_json ~context:"Action" json
  let yojson_of_t action = `String action

  type one_or_many = t list [@@deriving show, eq]

  let yojson_of_one_or_many actions = `List (List.map actions ~f:yojson_of_t)

  let one_or_many_of_json json =
    Decode.one_or_many_of_json ~context:"Action" of_json json
end

module Statement = struct
  type t = {
    effect_ : Effect.t; [@yojson.key "Effect"]
    principal : Principal.t; [@yojson.key "Principal"]
    actions : Action.one_or_many; [@yojson.key "Action"]
    resources : Resource.one_or_many; [@yojson.key "Resource"]
  }
  [@@deriving show, eq, yojson_of]

  let of_json = function
    | `Assoc fields ->
        let open Decode.Let_syntax in
        let* effect_json = Decode.field fields "Effect" in
        let* principal_json = Decode.field fields "Principal" in
        let* actions_json = Decode.field fields "Action" in
        let* resources_json = Decode.field fields "Resource" in
        let* effect_ = Effect.of_json effect_json in
        let* principal = Principal.of_json principal_json in
        let* actions = Action.one_or_many_of_json actions_json in
        let+ resources = Resource.one_or_many_of_json resources_json in
        { effect_; principal; actions; resources }
    | _ -> Decode.invalid_json "Statement must be an object"
end

type t = {
  version : string; [@yojson.key "Version"]
  statements : Statement.t list; [@yojson.key "Statement"]
}
[@@deriving show, eq, yojson_of]

let to_json policy = Yojson.Safe.to_string (yojson_of_t policy)

let statements_of_json json =
  Decode.one_or_many_of_json ~context:"Statement" Statement.of_json json

let of_json_value = function
  | `Assoc fields ->
      let open Decode.Let_syntax in
      let version =
        match Decode.field_opt fields "Version" with
        | None -> Ok "2012-10-17"
        | Some json -> Decode.string_of_json ~context:"Version" json
      in
      let* version = version in
      let* statements_json = Decode.field fields "Statement" in
      let+ statements = statements_of_json statements_json in
      { version; statements }
  | _ -> Decode.invalid_json "policy must be a JSON object"

let of_json s =
  let open Decode.Let_syntax in
  let* json = Decode.json_of_string s in
  of_json_value json

let action_matches ~pattern ~action =
  String.equal pattern "s3:*" || String.equal pattern action

let evaluate policy ~access_key_id ~action ~target =
  let resource_matches resource = Resource.matches ~resource ~target in
  let statement_matches (statement : Statement.t) =
    let principal_matches =
      Principal.matches ~principal:statement.principal ~access_key_id
    in
    let action_matches =
      List.exists statement.actions ~f:(fun pattern ->
          action_matches ~pattern ~action)
    in
    let resource_matches =
      List.exists statement.resources ~f:resource_matches
    in
    if principal_matches && action_matches && resource_matches then
      Some statement.effect_
    else None
  in
  let results = List.filter_map policy.statements ~f:statement_matches in
  if List.exists results ~f:(Effect.equal Effect.Deny) then `Denied
  else if List.exists results ~f:(Effect.equal Effect.Allow) then `Allowed
  else `No_match
