open Awskit_s3

let to_alcotest = Awskit_test.Qcheck.to_alcotest

let expect_error_field label field = function
  | Ok _ -> Alcotest.failf "%s: expected validation field %s" label field
  | Error error ->
      Alcotest.(check (option string))
        (label ^ " validation field")
        (Some field)
        (Awskit.Error.validation_field error)

let upload_parts ~content_length ~part_size =
  Transfer.Plan.upload_parts ~content_length ~part_size |> Result.get_ok

let download_ranges ~content_length ~part_size =
  Transfer.Plan.download_ranges ~content_length ~part_size |> Result.get_ok

let upload_part_seq ~content_length ~part_size =
  Transfer.Plan.upload_part_seq ~content_length ~part_size
  |> Result.get_ok
  |> List.of_seq

let download_range_seq ~content_length ~part_size =
  Transfer.Plan.download_range_seq ~content_length ~part_size
  |> Result.get_ok
  |> List.of_seq

let part_numbers parts =
  List.map
    (fun (part : Transfer.Plan.upload_part) ->
      Multipart.Part_number.to_int part.part_number)
    parts

let upload_lengths parts =
  List.map (fun (part : Transfer.Plan.upload_part) -> part.length) parts

let download_lengths ranges =
  List.map (fun (range : Transfer.Plan.download_range) -> range.length) ranges

let test_upload_planning_edges () =
  expect_error_field "zero upload" "content_length"
    (Transfer.Plan.upload_parts ~content_length:0L
       ~part_size:Transfer.min_part_size);
  let one =
    upload_parts
      ~content_length:(Int64.of_int Transfer.min_part_size)
      ~part_size:Transfer.min_part_size
  in
  Alcotest.(check (list int)) "one part number" [ 1 ] (part_numbers one);
  Alcotest.(check (list int))
    "one part length" [ Transfer.min_part_size ] (upload_lengths one);
  let three =
    upload_parts
      ~content_length:
        (Int64.add (Int64.mul 2L (Int64.of_int Transfer.min_part_size)) 17L)
      ~part_size:Transfer.min_part_size
  in
  Alcotest.(check (list int)) "part numbers" [ 1; 2; 3 ] (part_numbers three);
  Alcotest.(check (list int))
    "part lengths"
    [ Transfer.min_part_size; Transfer.min_part_size; 17 ]
    (upload_lengths three)

let test_upload_planning_rejects_too_many_parts () =
  let content_length =
    Int64.mul
      (Int64.of_int (Transfer.max_parts + 1))
      (Int64.of_int Transfer.min_part_size)
  in
  expect_error_field "too many upload parts" "part_count"
    (Transfer.Plan.upload_parts ~content_length
       ~part_size:Transfer.min_part_size)

let test_download_planning_edges () =
  Alcotest.(check int)
    "empty download ranges" 0
    (List.length
       (download_ranges ~content_length:0L ~part_size:Transfer.min_part_size));
  let ranges = download_ranges ~content_length:10L ~part_size:4 in
  Alcotest.(check (list int))
    "range lengths" [ 4; 4; 2 ] (download_lengths ranges);
  Alcotest.(check (list string))
    "range headers"
    [ "bytes=0-3"; "bytes=4-7"; "bytes=8-9" ]
    (List.map
       (fun (range : Transfer.Plan.download_range) ->
         Range.to_header range.range)
       ranges)

let test_download_planning_rejects_too_many_ranges () =
  expect_error_field "too many download ranges" "part_count"
    (Transfer.Plan.download_ranges
       ~content_length:(Int64.of_int (Transfer.max_parts + 1))
       ~part_size:1)

let test_lazy_plans_match_list_plans () =
  let upload_length =
    Int64.add (Int64.mul 2L (Int64.of_int Transfer.min_part_size)) 23L
  in
  let upload_list =
    upload_parts ~content_length:upload_length ~part_size:Transfer.min_part_size
  in
  let upload_seq =
    upload_part_seq ~content_length:upload_length
      ~part_size:Transfer.min_part_size
  in
  Alcotest.(check (list int))
    "lazy upload part numbers" (part_numbers upload_list)
    (part_numbers upload_seq);
  Alcotest.(check (list int))
    "lazy upload lengths"
    (upload_lengths upload_list)
    (upload_lengths upload_seq);
  let download_list = download_ranges ~content_length:11L ~part_size:4 in
  let download_seq = download_range_seq ~content_length:11L ~part_size:4 in
  Alcotest.(check (list int))
    "lazy download lengths"
    (download_lengths download_list)
    (download_lengths download_seq);
  Alcotest.(check (list string))
    "lazy download ranges"
    (List.map
       (fun (range : Transfer.Plan.download_range) ->
         Range.to_header range.range)
       download_list)
    (List.map
       (fun (range : Transfer.Plan.download_range) ->
         Range.to_header range.range)
       download_seq)

let sum_lengths lengths =
  List.fold_left (fun total length -> total + length) 0 lengths

let prop_upload_parts_cover_content =
  let gen =
    QCheck.Gen.(
      pair
        (int_range 1 (Transfer.min_part_size * 3))
        (int_range Transfer.min_part_size (Transfer.min_part_size * 2)))
  in
  QCheck.Test.make ~count:200
    ~name:"upload plan covers generated content exactly"
    (QCheck.make
       ~print:(fun (content_length, part_size) ->
         Fmt.str "content_length=%d part_size=%d" content_length part_size)
       gen)
    (fun (content_length, part_size) ->
      match
        Transfer.Plan.upload_parts
          ~content_length:(Int64.of_int content_length)
          ~part_size
      with
      | Error _ -> false
      | Ok parts -> (
          let offsets =
            List.map
              (fun (part : Transfer.Plan.upload_part) -> part.offset)
              parts
          in
          let expected_offsets =
            let rec loop offset acc = function
              | [] -> List.rev acc
              | length :: rest ->
                  loop
                    (Int64.add offset (Int64.of_int length))
                    (offset :: acc) rest
            in
            loop 0L [] (upload_lengths parts)
          in
          List.equal Int64.equal offsets expected_offsets
          && List.equal Int.equal (part_numbers parts)
               (List.init (List.length parts) (fun index -> index + 1))
          && sum_lengths (upload_lengths parts) = content_length
          && List.for_all
               (fun (part : Transfer.Plan.upload_part) ->
                 Multipart.Part_number.to_int part.part_number >= 1
                 && Multipart.Part_number.to_int part.part_number
                    <= Transfer.max_parts)
               parts
          &&
          match List.rev parts with
          | [] -> false
          | _final :: non_final_rev ->
              List.for_all
                (fun (part : Transfer.Plan.upload_part) ->
                  part.length >= Transfer.min_part_size)
                non_final_rev))

let prop_download_ranges_cover_content =
  let gen = QCheck.Gen.(pair (int_range 0 50_000) (int_range 1 4096)) in
  QCheck.Test.make ~count:200
    ~name:"download plan covers generated content exactly"
    (QCheck.make
       ~print:(fun (content_length, part_size) ->
         Fmt.str "content_length=%d part_size=%d" content_length part_size)
       gen)
    (fun (content_length, part_size) ->
      match
        Transfer.Plan.download_ranges
          ~content_length:(Int64.of_int content_length)
          ~part_size
      with
      | Error _ -> false
      | Ok ranges ->
          let expected_offsets =
            let rec loop offset acc = function
              | [] -> List.rev acc
              | length :: rest ->
                  loop
                    (Int64.add offset (Int64.of_int length))
                    (offset :: acc) rest
            in
            loop 0L [] (download_lengths ranges)
          in
          List.equal Int64.equal
            (List.map
               (fun (range : Transfer.Plan.download_range) -> range.offset)
               ranges)
            expected_offsets
          && sum_lengths (download_lengths ranges) = content_length
          && List.for_all
               (fun (range : Transfer.Plan.download_range) ->
                 match Range.view range.range with
                 | Range.Bytes (start, finish) ->
                     Int64.equal start range.offset
                     && Int64.equal finish
                          (Int64.add range.offset
                             (Int64.of_int (range.length - 1)))
                 | _ -> false)
               ranges)

let suite =
  [
    ( "transfer plan",
      [
        Alcotest.test_case "upload planning edges" `Quick
          test_upload_planning_edges;
        Alcotest.test_case "upload planning rejects too many parts" `Quick
          test_upload_planning_rejects_too_many_parts;
        Alcotest.test_case "download planning edges" `Quick
          test_download_planning_edges;
        Alcotest.test_case "download planning rejects too many ranges" `Quick
          test_download_planning_rejects_too_many_ranges;
        Alcotest.test_case "lazy plans match list plans" `Quick
          test_lazy_plans_match_list_plans;
      ] );
    ( "pbt:awskit-s3:transfer-plan",
      [
        to_alcotest prop_upload_parts_cover_content;
        to_alcotest prop_download_ranges_cover_content;
      ] );
  ]
