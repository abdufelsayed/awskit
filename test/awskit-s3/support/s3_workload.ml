module Command = S3_command
module Model = S3_model

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

module Make (Target : TARGET) = struct
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

  let property =
    QCheck.Test.make ~count:150
      ~name:(Printf.sprintf "%s S3 workload follows model" Target.name)
      Command.arbitrary run

  let suite =
    [
      ( Printf.sprintf "workload:%s:s3-state" Target.name,
        [ QCheck_alcotest.to_alcotest ~speed_level:`Quick property ] );
    ]
end
