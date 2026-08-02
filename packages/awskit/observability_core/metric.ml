open Base

module Number = struct
  type _ t = Int : int t | Int64 : int64 t | Float : float t
end

module Labels = struct
  type 'labels label =
    | Label :
        'value Fields.Dimension.Enum.t * ('labels -> 'value)
        -> 'labels label

  type 'labels t = 'labels label list

  let empty () = []
  let add dimension ~get labels = labels @ [ Label (dimension, get) ]

  let info labels =
    List.map labels ~f:(fun (Label (dimension, _)) ->
        ( Fields.Dimension.Enum.name dimension,
          Fields.Dimension.Enum.allowed_values dimension ))

  let encode labels value =
    List.map labels ~f:(fun (Label (dimension, get)) ->
        Fields.Dimension.Enum.encode dimension (get value))
end

module Family = struct
  type counter
  type histogram
  type gauge
  type aggregation = Counter | Histogram | Gauge
  type number = Int | Int64 | Float

  type ('aggregation, 'labels, 'value) t = {
    id : int;
    name : string;
    doc : string;
    unit_ : string option;
    aggregation : aggregation;
    labels : 'labels Labels.t;
    number : 'value Number.t;
  }

  type packed = Family : ('aggregation, 'labels, 'value) t -> packed

  let next_id = Atomic.make 0

  let create ~aggregation ~name ~doc ?unit_ ~labels ~value () =
    if not (String.is_prefix name ~prefix:"awskit.") then
      invalid_arg "Awskit metric family names must begin with awskit.";
    if String.is_empty doc then
      invalid_arg "Awskit metric family documentation must not be empty";
    {
      id = Atomic.fetch_and_add next_id 1;
      name;
      doc;
      unit_;
      aggregation;
      labels;
      number = value;
    }

  let counter ~name ~doc ?unit_ ~labels ~value () =
    (create ~aggregation:Counter ~name ~doc ?unit_ ~labels ~value ()
      : (counter, _, _) t)

  let histogram ~name ~doc ?unit_ ~labels ~value () =
    (create ~aggregation:Histogram ~name ~doc ?unit_ ~labels ~value ()
      : (histogram, _, _) t)

  let gauge ~name ~doc ?unit_ ~labels ~value () =
    (create ~aggregation:Gauge ~name ~doc ?unit_ ~labels ~value ()
      : (gauge, _, _) t)

  let pack family = Family family
  let id (Family family) = family.id
  let name (Family family) = family.name
  let doc (Family family) = family.doc
  let unit_ (Family family) = family.unit_
  let aggregation (Family family) = family.aggregation

  let number (Family family) =
    match family.number with
    | Number.Int -> Int
    | Number.Int64 -> Int64
    | Number.Float -> Float

  let labels (Family family) = Labels.info family.labels
  let equal left right = Int.equal (id left) (id right)
end

module Value = struct
  type t = Int of int | Int64 of int64 | Float of float

  let of_typed : type value. value Number.t -> value -> t =
   fun kind value ->
    match kind with
    | Number.Int -> Int value
    | Number.Int64 -> Int64 value
    | Number.Float -> Float value
end

module Label = struct
  type t = { name : string; allowed_values : string list }
  type value = { label : t; encoded : string }

  let create ~name ~allowed_values = { name; allowed_values }
  let name t = t.name
  let allowed_values t = t.allowed_values
  let label t = t.label
  let encoded t = t.encoded
end

module Observation = struct
  type t = {
    family : Family.packed;
    labels : Label.value list;
    value : Value.t;
  }

  let family t = t.family
  let labels t = t.labels
  let value t = t.value

  let create family labels value =
    let label_info = Labels.info family.Family.labels in
    let encoded = Labels.encode family.labels labels in
    let labels =
      List.map2_exn label_info encoded ~f:(fun (name, allowed_values) encoded ->
          { Label.label = { name; allowed_values }; encoded })
    in
    {
      family = Family.pack family;
      labels;
      value = Value.of_typed family.number value;
    }
end

module Sink = struct
  type t = {
    name : string;
    needs_clock : bool;
    enabled : Family.packed -> bool;
    observe : Observation.t -> unit;
  }

  let create ~name ~needs_clock ~enabled ~observe =
    if String.is_empty name then
      invalid_arg "Awskit metric sink name must not be empty";
    { name; needs_clock; enabled; observe }

  let name t = t.name
  let needs_clock t = t.needs_clock
  let enabled t = t.enabled
  let observe t = t.observe
end

module Projection = struct
  type 'input t =
    | Projection : {
        family : ('aggregation, 'labels, 'value) Family.t;
        needs_duration : bool;
        get : 'input -> ('labels * 'value) option;
      }
        -> 'input t

  let sample ?(needs_duration = false) family ~get =
    Projection { family; needs_duration; get }

  let family (Projection projection) = Family.pack projection.family
  let needs_duration (Projection projection) = projection.needs_duration

  let observe (Projection projection) input =
    Option.map (projection.get input) ~f:(fun (labels, value) ->
        Observation.create projection.family labels value)
end

type counter = Family.counter
type histogram = Family.histogram
type gauge = Family.gauge
