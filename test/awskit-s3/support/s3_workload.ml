module Command = S3_command
module Model = S3_model

type profile = S3_history.target_profile = Strict | Minio
type integration_profile = Bounded | Expensive

type workload_profile = {
  name : string;
  target_profile : profile;
  history_config : S3_history.config;
  default_count : int;
}

let supports_command = S3_history.supports_command

let integration_profile_to_string = function
  | Bounded -> "bounded"
  | Expensive -> "expensive"

let integration_profile_allowed_values =
  [
    integration_profile_to_string Bounded;
    integration_profile_to_string Expensive;
  ]

type integration_profile_parse_error = Unknown_integration_profile of string

let integration_profile_of_string = function
  | "bounded" -> Ok Bounded
  | "expensive" -> Ok Expensive
  | value -> Error (Unknown_integration_profile value)

let minio_workload_profile integration_profile =
  (* Both integration profiles target the local MinIO test double. Expensive
     raises cost and generated value/history breadth; it is not a broader
     S3-compatible provider support claim. *)
  match integration_profile with
  | Bounded ->
      {
        name = integration_profile_to_string Bounded;
        target_profile = Minio;
        history_config = S3_history.minio_default;
        default_count = 25;
      }
  | Expensive ->
      {
        name = integration_profile_to_string Expensive;
        target_profile = Minio;
        history_config = S3_history.minio_expensive;
        default_count = 100;
      }

let strict_workload_profile ~count =
  {
    name = S3_history.target_profile_to_string Strict;
    target_profile = Strict;
    history_config = S3_history.strict_default;
    default_count = count;
  }

let count_from_env ~var ~default =
  match Sys.getenv_opt var with
  | None | Some "" -> default
  | Some value -> (
      match int_of_string_opt value with
      | Some count when count > 0 -> count
      | _ -> default)

module type TARGET = sig
  val name : string
  val bucket : Awskit_s3.Bucket_name.t

  type connection

  (* Targets own setup and teardown because external runners may need to clean
     up partially acquired resources if setup fails. Teardown should preserve
     the primary workload failure instead of replacing it. *)
  val with_connection : (connection -> 'a) -> 'a
  val check_store : int -> Command.t -> connection -> Model.t -> unit
  val run_command : int -> connection -> Model.t -> Command.t -> Model.t
end

let arbitrary_for_workload_profile profile =
  S3_history.arbitrary ~label:profile.name profile.history_config

let required_strict_bins =
  [
    "s3.command.put";
    "s3.command.put-metadata";
    "s3.command.get";
    "s3.command.get-range";
    "s3.command.find";
    "s3.command.head";
    "s3.command.exists";
    "s3.command.delete";
    "s3.command.list";
    "s3.command.list-prefix";
    "s3.command.list-keys-page";
    "s3.command.list-versions-page";
    "s3.command.copy";
    "s3.command.copy-metadata";
    "s3.command.put-object-tags";
    "s3.command.get-object-tags";
    "s3.command.delete-object-tags";
    "s3.command.put-bucket-tags";
    "s3.command.get-bucket-tags";
    "s3.command.delete-bucket-tags";
    "s3.command.versioning.enabled";
    "s3.command.versioning.suspended";
    "s3.command.get-versioning";
    "s3.history.put-read";
    "s3.history.put-range-read";
    "s3.history.put-delete";
    "s3.history.versioning-after-put";
    "s3.history.delete-after-versioning";
    "s3.history.versioning-enabled-after-existing-object";
    "s3.history.versioning-enabled-delete";
    "s3.history.object-tag-mutation-read";
    "s3.history.copy-destination-read";
    "s3.history.list-page-after-multiple-visible-keys";
    "s3.state.empty";
    "s3.state.one-object";
    "s3.state.multiple-objects";
    "s3.state.versioning-enabled";
    "s3.state.versioning-suspended";
    "s3.state.delete-marker-present";
    "s3.state.bucket-tags-present";
    "s3.state.object-tags-present";
  ]

let test_generator_coverage generator () =
  let histories = QCheck.Gen.generate ~n:500 generator in
  let coverage =
    Workload_coverage.of_lists histories ~bins:S3_history.coverage_bins
  in
  Workload_coverage.require_all ~label:"S3 workload generator"
    ~required:required_strict_bins coverage

module Make_with_profile
    (Config : sig
      val workload_profile : workload_profile
    end)
    (Target : TARGET) =
struct
  let run commands =
    Target.with_connection (fun conn ->
        Target.check_store 0 Command.List_keys conn Model.empty;
        let (_ : Model.t) =
          List.fold_left
            (fun model (index, command) ->
              let next_model = Target.run_command index conn model command in
              Target.check_store index command conn next_model;
              next_model)
            Model.empty
            (List.mapi (fun index command -> (index + 1, command)) commands)
        in
        true)

  let count =
    count_from_env ~var:"AWSKIT_QCHECK_COUNT"
      ~default:Config.workload_profile.default_count

  let property =
    QCheck.Test.make ~count
      ~name:
        (Printf.sprintf "%s S3 workload follows model (%s profile)" Target.name
           Config.workload_profile.name)
      (arbitrary_for_workload_profile Config.workload_profile)
      run

  let coverage_cases =
    match Config.workload_profile.target_profile with
    | Strict ->
        [
          Alcotest.test_case "generator semantic coverage" `Quick
            (test_generator_coverage
               (S3_history.generator Config.workload_profile.history_config));
        ]
    | Minio -> []

  let suite =
    [
      ( Printf.sprintf "workload:%s:s3-state" Target.name,
        coverage_cases
        @ [ QCheck_alcotest.to_alcotest ~speed_level:`Quick property ] );
    ]
end

module Make (Target : TARGET) =
  Make_with_profile
    (struct
      let workload_profile = strict_workload_profile ~count:150
    end)
    (Target)
