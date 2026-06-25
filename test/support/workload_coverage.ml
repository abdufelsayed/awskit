module String_map = Map.Make (String)

type t = int String_map.t

let empty = String_map.empty

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

let require_all ~label ~required coverage =
  match missing required coverage with
  | [] -> ()
  | bins ->
      Alcotest.failf "%s missing semantic coverage bins:\n%s\n\nobserved:\n%s"
        label (String.concat "\n" bins) (pp coverage)
