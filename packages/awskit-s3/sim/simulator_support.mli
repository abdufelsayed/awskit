open Awskit_s3

val ( let* ) :
  ('a, Awskit.Error.t) result ->
  ('a -> ('b, Awskit.Error.t) result) ->
  ('b, Awskit.Error.t) result

val invalid :
  ?field:string ->
  ('a, Format.formatter, unit, ('b, Awskit.Error.t) result) format4 ->
  'a

val decode : ('a, Format.formatter, unit, Awskit.Error.t) format4 -> 'a
val is_prefix : prefix:string -> string -> bool
val ptime_to_header : Ptime.t -> string

val validate_header_value :
  field:string -> string -> (unit, Awskit.Error.t) result

val validate_metadata : Awskit_s3.Metadata.t -> (unit, Awskit.Error.t) result
val validate_tags : Awskit_s3.Tag.Set.t -> (unit, Awskit.Error.t) result
val validate_bucket : string -> (unit, Awskit.Error.t) result
val validate_key : string -> (unit, Awskit.Error.t) result
val validate_bucket_key : string -> string -> (unit, Awskit.Error.t) result
