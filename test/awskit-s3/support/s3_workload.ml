module Command = S3_command
module Model = S3_model

type profile = Strict | Minio

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

let supports_command profile command =
  match (profile, command) with
  | Strict, _ -> true
  | Minio, Command.Put_versioning status -> (
      match status with
      | Awskit_s3.Bucket.Versioning.Status.Suspended -> false
      | Enabled | Unknown _ -> true)
  | Minio, Command.Copy_object (source_key, destination_key) ->
      not (String.equal source_key destination_key)
  | Minio, _ -> true

let command_supported profile command = supports_command profile command

let commands_supported profile =
  List.for_all (fun command -> command_supported profile command)

let supported_generators profile =
  let open QCheck.Gen in
  let maybe command gen =
    if command_supported profile command then Some gen else None
  in
  let copy_generator =
    match profile with
    | Strict ->
        Some
          (map2
             (fun source destination ->
               Command.Copy_object (source, destination))
             Command.gen_key Command.gen_key)
    | Minio ->
        let pairs =
          List.concat_map
            (fun source ->
              List.filter_map
                (fun destination ->
                  if String.equal source destination then None
                  else Some (source, destination))
                Command.key_domain)
            Command.key_domain
        in
        if pairs = [] then None
        else
          Some
            (map
               (fun (source, destination) ->
                 Command.Copy_object (source, destination))
               (oneof_list pairs))
  in
  let versioning_generator =
    let statuses =
      List.filter
        (fun status ->
          command_supported profile (Command.Put_versioning status))
        Command.versioning_status_domain
    in
    match statuses with
    | [] -> None
    | statuses ->
        Some
          (map
             (fun status -> Command.Put_versioning status)
             (oneof_list statuses))
  in
  let generators =
    [
      maybe
        (Command.Put_string ("a.txt", "", []))
        (map3
           (fun key body tags -> Command.Put_string (key, body, tags))
           Command.gen_key Command.gen_body Command.gen_tags);
      maybe (Command.Get_string "a.txt")
        (map (fun key -> Command.Get_string key) Command.gen_key);
      maybe (Command.Find_string "a.txt")
        (map (fun key -> Command.Find_string key) Command.gen_key);
      maybe (Command.Head_object "a.txt")
        (map (fun key -> Command.Head_object key) Command.gen_key);
      maybe (Command.Exists_object "a.txt")
        (map (fun key -> Command.Exists_object key) Command.gen_key);
      maybe (Command.Delete_object "a.txt")
        (map (fun key -> Command.Delete_object key) Command.gen_key);
      maybe Command.List_keys (return Command.List_keys);
      maybe (Command.List_prefix "a")
        (map (fun prefix -> Command.List_prefix prefix) Command.gen_prefix);
      copy_generator;
      maybe
        (Command.Put_object_tags ("a.txt", []))
        (map2
           (fun key tags -> Command.Put_object_tags (key, tags))
           Command.gen_key Command.gen_tags);
      maybe (Command.Get_object_tags "a.txt")
        (map (fun key -> Command.Get_object_tags key) Command.gen_key);
      maybe (Command.Delete_object_tags "a.txt")
        (map (fun key -> Command.Delete_object_tags key) Command.gen_key);
      maybe (Command.Put_bucket_tags [])
        (map (fun tags -> Command.Put_bucket_tags tags) Command.gen_tags);
      maybe Command.Get_bucket_tags (return Command.Get_bucket_tags);
      maybe Command.Delete_bucket_tags (return Command.Delete_bucket_tags);
      versioning_generator;
      maybe Command.Get_versioning (return Command.Get_versioning);
    ]
  in
  List.filter_map Fun.id generators

let arbitrary_for_profile profile =
  match profile with
  | Strict -> Command.arbitrary
  | Minio ->
      let command =
        match supported_generators profile with
        | [] ->
            QCheck.Gen.map
              (fun () ->
                QCheck.Test.fail_reportf "S3 workload profile has no commands")
              QCheck.Gen.unit
        | generators -> QCheck.Gen.oneof generators
      in
      let shrink commands =
        Command.shrink_list commands
        |> QCheck.Iter.filter (commands_supported profile)
      in
      QCheck.make ~print:Command.transcript ~shrink
        QCheck.Gen.(list_size (int_range 1 20) command)

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

  let suite =
    [
      ( Printf.sprintf "workload:%s:s3-state" Target.name,
        [ QCheck_alcotest.to_alcotest ~speed_level:`Quick property ] );
    ]
end

module Make (Target : TARGET) =
  Make_with_config
    (struct
      let profile = Strict
      let count = 150
    end)
    (Target)

module Make_profile
    (Config : sig
      val profile : profile
    end)
    (Target : TARGET) =
  Make_with_config
    (struct
      let profile = Config.profile
      let count = match profile with Strict -> 150 | Minio -> 50
    end)
    (Target)
