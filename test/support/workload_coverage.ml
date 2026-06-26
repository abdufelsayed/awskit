module String_map = Map.Make (String)

type t = int String_map.t
type threshold = { bin : string; minimum : int }

let empty = String_map.empty
let threshold ~bin ~minimum = { bin; minimum }

let add bin coverage =
  String_map.update bin
    (function None -> Some 1 | Some count -> Some (count + 1))
    coverage

let add_many bins coverage =
  List.fold_left (fun acc bin -> add bin acc) coverage bins

let of_lists inputs ~bins =
  List.fold_left (fun acc input -> add_many (bins input) acc) empty inputs

let count bin coverage =
  Option.value ~default:0 (String_map.find_opt bin coverage)

let has bin coverage = count bin coverage > 0

let missing required coverage =
  List.filter (fun bin -> not (has bin coverage)) required

let to_lines coverage =
  coverage
  |> String_map.bindings
  |> List.map (fun (bin, count) -> Printf.sprintf "%s=%d" bin count)

let pp coverage = String.concat "\n" (to_lines coverage)

let below_threshold coverage { bin; minimum } =
  let observed = count bin coverage in
  if observed < minimum then Some (bin, observed, minimum) else None

let threshold_failures ~thresholds coverage =
  List.filter_map (below_threshold coverage) thresholds

let threshold_failure_lines failures =
  List.map
    (fun (bin, observed, minimum) ->
      Printf.sprintf "%s observed=%d minimum=%d" bin observed minimum)
    failures

let require_all ~label ~required coverage =
  match missing required coverage with
  | [] -> ()
  | bins ->
      Alcotest.failf "%s missing semantic coverage bins:\n%s\n\nobserved:\n%s"
        label (String.concat "\n" bins) (pp coverage)

let require_thresholds ~label ~sample_count ~thresholds coverage =
  match threshold_failures ~thresholds coverage with
  | [] -> ()
  | failures ->
      Alcotest.failf
        "%s weak semantic coverage across %d samples:\n%s\n\nobserved:\n%s"
        label sample_count
        (String.concat "\n" (threshold_failure_lines failures))
        (pp coverage)

let adjacent_pairs ~bin values =
  let rec loop acc = function
    | left :: right :: rest ->
        loop
          (Printf.sprintf "%s->%s" (bin left) (bin right) :: acc)
          (right :: rest)
    | [] | [ _ ] -> List.rev acc
  in
  loop [] values
