open Base

module Info = struct
  type t = {
    name : string;
    doc : string;
    source : Logs.src;
    span_kind : Fields.Span_kind.t;
  }

  let create ~name ~doc ~source ~span_kind =
    if not (String.is_prefix name ~prefix:"awskit.") then
      invalid_arg "Awskit observation names must begin with awskit.";
    if String.is_empty doc then
      invalid_arg "Awskit observation documentation must not be empty";
    { name; doc; source; span_kind }

  let name t = t.name
  let doc t = t.doc
  let source t = t.source
  let span_kind t = t.span_kind
end

module Safe_fields = struct
  type t = {
    dimensions : Fields.Dimension.t list;
    measurements : Fields.Measurement.t list;
    diagnostics : Fields.Diagnostic.Public.t list;
  }

  let unique_by_name values ~name =
    let _, values =
      List.fold_right values
        ~init:(Set.empty (module String), [])
        ~f:(fun value (names, unique) ->
          let name = name value in
          if Set.mem names name then (names, unique)
          else (Set.add names name, value :: unique))
    in
    values

  let create ~dimensions ~measurements ~diagnostics =
    {
      dimensions = unique_by_name dimensions ~name:Fields.Dimension.name;
      measurements = unique_by_name measurements ~name:Fields.Measurement.name;
      diagnostics =
        unique_by_name diagnostics ~name:Fields.Diagnostic.Public.name;
    }

  let of_fields fields =
    create
      ~dimensions:(Fields.Fields.dimensions fields)
      ~measurements:(Fields.Fields.measurements fields)
      ~diagnostics:(Fields.Fields.diagnostics fields)

  let append left right =
    create
      ~dimensions:(left.dimensions @ right.dimensions)
      ~measurements:(left.measurements @ right.measurements)
      ~diagnostics:(left.diagnostics @ right.diagnostics)

  let add_correlation t correlation =
    {
      t with
      diagnostics =
        unique_by_name
          (t.diagnostics @ correlation)
          ~name:Fields.Diagnostic.Public.name;
    }
end

module Operation = struct
  module Start = struct
    type t = { info : Info.t; fields : Safe_fields.t }

    let create ~info ~fields = { info; fields = Safe_fields.of_fields fields }
    let info t = t.info
    let dimensions t = t.fields.dimensions
    let measurements t = t.fields.measurements
    let diagnostics t = t.fields.diagnostics
  end

  module Completion = struct
    type t = {
      info : Info.t;
      outcome : Fields.Outcome.t;
      duration_ns : int64 option;
      fields : Safe_fields.t;
    }

    let create ~info ~outcome ~duration_ns ~start ~finish ~inherited
        ~correlation =
      let fields =
        Safe_fields.append inherited
          (Safe_fields.append
             (Safe_fields.of_fields start)
             (Safe_fields.of_fields finish))
        |> fun fields -> Safe_fields.add_correlation fields correlation
      in
      { info; outcome; duration_ns; fields }

    let info t = t.info
    let outcome t = t.outcome
    let duration_ns t = t.duration_ns
    let dimensions t = t.fields.dimensions
    let measurements t = t.fields.measurements
    let diagnostics t = t.fields.diagnostics

    let pp ppf t =
      Fmt.pf ppf "%s outcome=%a" (Info.name t.info) Fields.Outcome.pp t.outcome;
      Option.iter t.duration_ns ~f:(Fmt.pf ppf " duration_ns=%Ld");
      List.iter t.fields.dimensions ~f:(Fmt.pf ppf " %a" Fields.Dimension.pp);
      List.iter t.fields.measurements
        ~f:(Fmt.pf ppf " %a" Fields.Measurement.pp);
      List.iter t.fields.diagnostics
        ~f:(Fmt.pf ppf " %a" Fields.Diagnostic.Public.pp)
  end
end

module Event = struct
  module Info = struct
    type t = { name : string; doc : string; source : Logs.src }

    let create ~name ~doc ~source =
      if not (String.is_prefix name ~prefix:"awskit.") then
        invalid_arg "Awskit event names must begin with awskit.";
      if String.is_empty doc then
        invalid_arg "Awskit event documentation must not be empty";
      { name; doc; source }

    let name t = t.name
    let doc t = t.doc
    let source t = t.source
  end

  type t = { info : Info.t; fields : Safe_fields.t }

  let create ~info ~fields ~inherited ~correlation =
    let fields =
      Safe_fields.append inherited (Safe_fields.of_fields fields)
      |> fun fields -> Safe_fields.add_correlation fields correlation
    in
    { info; fields }

  let info t = t.info
  let dimensions t = t.fields.dimensions
  let measurements t = t.fields.measurements
  let diagnostics t = t.fields.diagnostics

  let pp ppf t =
    Fmt.string ppf (Info.name t.info);
    List.iter t.fields.dimensions ~f:(Fmt.pf ppf " %a" Fields.Dimension.pp);
    List.iter t.fields.measurements ~f:(Fmt.pf ppf " %a" Fields.Measurement.pp);
    List.iter t.fields.diagnostics
      ~f:(Fmt.pf ppf " %a" Fields.Diagnostic.Public.pp)
end

module Scope = struct
  type t = Safe_fields.t

  let root =
    { Safe_fields.dimensions = []; measurements = []; diagnostics = [] }

  let of_start ~parent start ~correlation =
    Safe_fields.append parent
      {
        Safe_fields.dimensions = Operation.Start.dimensions start;
        measurements = Operation.Start.measurements start;
        diagnostics = Operation.Start.diagnostics start @ correlation;
      }

  let fields t = t
end
