module Command = S3_command
module Model = S3_model

type profile = S3_history.target_profile = Strict | Minio

let supports_command = S3_history.supports_command

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

let arbitrary_for_profile profile =
  match profile with
  | Strict -> S3_history.arbitrary S3_history.strict_default
  | Minio -> S3_history.arbitrary S3_history.minio_default

let required_strict_bins =
  [
    "s3.command.put";
    "s3.command.put-metadata";
    "s3.command.get";
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
    "s3.history.versioning-after-put";
    "s3.history.delete-after-versioning";
  ]

let test_generator_coverage generator () =
  let histories = QCheck.Gen.generate ~n:500 generator in
  let coverage =
    Workload_coverage.of_lists histories ~bins:S3_command.history_bins
  in
  Workload_coverage.require_all ~label:"S3 workload generator"
    ~required:required_strict_bins coverage

module Make_with_config
    (Config : sig
      val profile : profile
      val count : int
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

  let count = count_from_env ~var:"AWSKIT_QCHECK_COUNT" ~default:Config.count

  let property =
    QCheck.Test.make ~count
      ~name:(Printf.sprintf "%s S3 workload follows model" Target.name)
      (arbitrary_for_profile Config.profile)
      run

  let coverage_cases =
    match Config.profile with
    | Strict ->
        [
          Alcotest.test_case "generator semantic coverage" `Quick
            (test_generator_coverage
               (S3_history.generator S3_history.strict_default));
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
  Make_with_config
    (struct
      let profile = Strict
      let count = 150
    end)
    (Target)
