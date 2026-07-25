open Base

module Projection = struct
  type kind = Logs | Metric | Trace | Engine
  type t = { id : int; kind : kind; name : string }

  let create ~id ~kind ~name =
    if id < 0 then
      invalid_arg "Awskit observer health projection id must be non-negative";
    if String.is_empty name then
      invalid_arg "Awskit observer health projection name must not be empty";
    { id; kind; name }

  let id t = t.id
  let kind t = t.kind
  let name t = t.name
end

type phase = Enablement | Start | Finish | Event | Instrument | Context
type cell = { projection : Projection.t; phase : phase; count : int Atomic.t }
type t = { projections : Projection.t list; cells : cell list }
type failure = { projection : Projection.t; phase : phase; count : int64 }
type snapshot = { projections : Projection.t list; failures : failure list }

let phases = [ Enablement; Start; Finish; Event; Instrument; Context ]

let create projections =
  let cells : cell list =
    List.concat_map projections ~f:(fun projection ->
        List.map phases ~f:(fun phase ->
            ({ projection; phase; count = Atomic.make 0 } : cell)))
  in
  { projections; cells }

let empty = create []

let rec increment_saturating (cell : cell) =
  let current = Atomic.get cell.count in
  if Int.equal current Int.max_value then ()
  else if Atomic.compare_and_set cell.count current (current + 1) then ()
  else increment_saturating cell

let bump t projection phase =
  match
    List.find t.cells ~f:(fun cell ->
        Int.equal (Projection.id cell.projection) (Projection.id projection)
        && Poly.equal cell.phase phase)
  with
  | None -> ()
  | Some cell -> increment_saturating cell

let snapshot t =
  let failures =
    List.filter_map t.cells ~f:(fun cell ->
        let count = Atomic.get cell.count in
        if Int.equal count 0 then None
        else
          Some
            {
              projection = cell.projection;
              phase = cell.phase;
              count = Int64.of_int count;
            })
  in
  { projections = t.projections; failures }

let projection (failure : failure) = failure.projection
let phase (failure : failure) = failure.phase
let count (failure : failure) = failure.count
let projections (snapshot : snapshot) = snapshot.projections
let failures (snapshot : snapshot) = snapshot.failures
