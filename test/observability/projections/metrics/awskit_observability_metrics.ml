open Base
module O = Awskit.Observability
module P = O.For_projection.Metric

type packed_tags =
  | Tags : {
      spec : 'tags Metrics.Tags.t;
      apply : string list -> 'tags -> Metrics.tags;
    }
      -> packed_tags

let rec make_tags = function
  | [] ->
      Tags
        {
          spec = Metrics.Tags.[];
          apply =
            (fun values make ->
              match values with
              | [] -> make
              | _ -> invalid_arg "unexpected metric labels");
        }
  | label :: rest ->
      let (Tags tail) = make_tags rest in
      Tags
        {
          spec = Metrics.Tags.(string (P.Label.name label) :: tail.spec);
          apply =
            (fun values make ->
              match values with
              | value :: values -> tail.apply values (make value)
              | [] -> invalid_arg "missing metric label");
        }

let metric_field family = function
  | P.Value.Int value -> Metrics.int ?unit:(P.Family.unit_ family) "value" value
  | P.Value.Int64 value ->
      Metrics.int64 ?unit:(P.Family.unit_ family) "value" value
  | P.Value.Float value ->
      Metrics.float ?unit:(P.Family.unit_ family) "value" value

type source =
  | Source : {
      family : P.Family.t;
      metrics : ('tags, P.Value.t -> Metrics.Data.t) Metrics.src;
      apply : string list -> 'tags -> Metrics.tags;
    }
      -> source

let registered : (int, source) Hashtbl.t = Hashtbl.create (module Int)

let registered_names : (string, source) Hashtbl.t =
  Hashtbl.create (module String)

let explicitly_enabled = Atomic.make false

let same_label left right =
  String.equal (P.Label.name left) (P.Label.name right)
  && List.equal String.equal
       (P.Label.allowed_values left)
       (P.Label.allowed_values right)

let same_descriptor left right =
  String.equal (P.Family.name left) (P.Family.name right)
  && String.equal (P.Family.doc left) (P.Family.doc right)
  && Option.equal String.equal (P.Family.unit_ left) (P.Family.unit_ right)
  && Poly.equal (P.Family.aggregation left) (P.Family.aggregation right)
  && Poly.equal (P.Family.number left) (P.Family.number right)
  && List.equal same_label (P.Family.labels left) (P.Family.labels right)

let cache family source =
  Hashtbl.set registered ~key:(P.Family.id family) ~data:source;
  Hashtbl.set registered_names ~key:(P.Family.name family) ~data:source;
  source

let register family =
  match Hashtbl.find registered (P.Family.id family) with
  | Some source -> source
  | None -> (
      match Hashtbl.find registered_names (P.Family.name family) with
      | Some (Source existing as source) ->
          if same_descriptor family existing.family then cache family source
          else
            invalid_arg
              ("conflicting Awskit metric family descriptor: "
              ^ P.Family.name family)
      | None ->
          let (Tags tags) = make_tags (P.Family.labels family) in
          let metrics =
            Metrics.Src.v (P.Family.name family) ~doc:(P.Family.doc family)
              ~tags:tags.spec ~data:(fun value ->
                Metrics.Data.v [ metric_field family value ])
          in
          if Atomic.get explicitly_enabled then
            Metrics.Src.enable (Metrics.Src.Src metrics);
          let source = Source { family; metrics; apply = tags.apply } in
          cache family source)

let enabled family =
  let (Source source) = register family in
  Metrics.is_active source.metrics

let observe observation =
  let family = P.Observation.family observation in
  let values =
    P.Observation.labels observation |> List.map ~f:P.Label.encoded
  in
  let (Source source) = register family in
  if not (same_descriptor family source.family) then
    invalid_arg "metric family identity mismatch";
  Metrics.add source.metrics
    (fun make -> source.apply values make)
    (fun make -> make (P.Observation.value observation))

let poll_instruments observations =
  List.iter observations ~f:(fun observation ->
      match P.Observation.family observation |> P.Family.aggregation with
      | P.Family.Gauge -> observe observation
      | Counter | Histogram ->
          invalid_arg "instrument snapshot contained a non-gauge family")

let sources () =
  Hashtbl.data registered
  |> List.map ~f:(fun (Source source) -> Metrics.Src.Src source.metrics)

let enable () =
  Atomic.set explicitly_enabled true;
  List.iter (sources ()) ~f:Metrics.Src.enable

let disable () =
  Atomic.set explicitly_enabled false;
  List.iter (sources ()) ~f:Metrics.Src.disable

let sink =
  O.Metric_sink.create ~name:"metrics" ~needs_clock:true ~enabled ~observe
