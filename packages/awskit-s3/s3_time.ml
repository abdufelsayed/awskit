let weekday_of_http = function
  | "Sun" -> Some `Sun
  | "Mon" -> Some `Mon
  | "Tue" -> Some `Tue
  | "Wed" -> Some `Wed
  | "Thu" -> Some `Thu
  | "Fri" -> Some `Fri
  | "Sat" -> Some `Sat
  | _ -> None

let http_of_weekday = function
  | `Sun -> "Sun"
  | `Mon -> "Mon"
  | `Tue -> "Tue"
  | `Wed -> "Wed"
  | `Thu -> "Thu"
  | `Fri -> "Fri"
  | `Sat -> "Sat"

let weekday_equal left right =
  match (left, right) with
  | `Sun, `Sun
  | `Mon, `Mon
  | `Tue, `Tue
  | `Wed, `Wed
  | `Thu, `Thu
  | `Fri, `Fri
  | `Sat, `Sat ->
      true
  | _ -> false

let month_of_http = function
  | "Jan" -> Some 1
  | "Feb" -> Some 2
  | "Mar" -> Some 3
  | "Apr" -> Some 4
  | "May" -> Some 5
  | "Jun" -> Some 6
  | "Jul" -> Some 7
  | "Aug" -> Some 8
  | "Sep" -> Some 9
  | "Oct" -> Some 10
  | "Nov" -> Some 11
  | "Dec" -> Some 12
  | _ -> None

let http_of_month = function
  | 1 -> Some "Jan"
  | 2 -> Some "Feb"
  | 3 -> Some "Mar"
  | 4 -> Some "Apr"
  | 5 -> Some "May"
  | 6 -> Some "Jun"
  | 7 -> Some "Jul"
  | 8 -> Some "Aug"
  | 9 -> Some "Sep"
  | 10 -> Some "Oct"
  | 11 -> Some "Nov"
  | 12 -> Some "Dec"
  | _ -> None

let digits_at value start len =
  let rec loop index acc =
    if Int.equal index (start + len) then Some acc
    else
      let code = Char.code value.[index] - Char.code '0' in
      if code < 0 || code > 9 then None else loop (index + 1) ((acc * 10) + code)
  in
  if String.length value < start + len then None else loop start 0

let ptime_of_http_date value =
  if not (Int.equal (String.length value) 29) then None
  else if
    (not (Char.equal value.[3] ','))
    || (not (Char.equal value.[4] ' '))
    || (not (Char.equal value.[7] ' '))
    || (not (Char.equal value.[11] ' '))
    || (not (Char.equal value.[16] ' '))
    || (not (Char.equal value.[19] ':'))
    || (not (Char.equal value.[22] ':'))
    || (not (Char.equal value.[25] ' '))
    || not (S3_string.substring_equal value 26 "GMT")
  then None
  else
    let ( let* ) = S3_result.option_bind in
    let* weekday = weekday_of_http (String.sub value 0 3) in
    let* day = digits_at value 5 2 in
    let* month = month_of_http (String.sub value 8 3) in
    let* year = digits_at value 12 4 in
    let* hour = digits_at value 17 2 in
    let* minute = digits_at value 20 2 in
    let* second = digits_at value 23 2 in
    let date = (year, month, day) in
    let* day_start = Ptime.of_date_time (date, ((0, 0, 0), 0)) in
    if not (weekday_equal (Ptime.weekday day_start) weekday) then None
    else Ptime.of_date_time (date, ((hour, minute, second), 0))

let of_string value =
  match Ptime.of_rfc3339 ~strict:false value with
  | Ok (time, _, _) -> Some time
  | Error _ -> ptime_of_http_date value

let to_header value =
  let (year, month, day), ((hour, minute, second), _) =
    Ptime.to_date_time value
  in
  match http_of_month month with
  | None -> Ptime.to_rfc3339 value
  | Some month ->
      Fmt.str "%s, %02d %s %04d %02d:%02d:%02d GMT"
        (http_of_weekday (Ptime.weekday value))
        day month year hour minute second
