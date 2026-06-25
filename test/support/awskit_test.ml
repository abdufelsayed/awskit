module Time = struct
  let fixed = Ptime.of_date_time ((2026, 4, 8), ((12, 0, 0), 0)) |> Option.get
end

module Credentials = struct
  let basic =
    Awskit.Credentials.create_exn ~access_key_id:"AKID"
      ~secret_access_key:"SECRET" ()
end

module Expect = struct
  let ok_or_fail pp label = function
    | Ok value -> value
    | Error error -> Alcotest.failf "%s: %a" label pp error
end

module Qcheck = struct
  let to_alcotest test = QCheck_alcotest.to_alcotest ~speed_level:`Quick test
end

module Header = struct
  let find name headers = List.assoc_opt name headers
end

module String = struct
  let contains ~substring value =
    let substring_length = Stdlib.String.length substring in
    let value_length = Stdlib.String.length value in
    let rec loop index =
      index + substring_length <= value_length
      && (Stdlib.String.sub value index substring_length = substring
         || loop (index + 1))
    in
    substring_length = 0 || loop 0
end
