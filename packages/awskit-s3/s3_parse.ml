let int_of_string_opt value =
  try Some (int_of_string value) with Failure _ -> None

let int64_of_string_opt value =
  try Some (Int64.of_string value) with Failure _ -> None

let is_decimal value =
  (not (String.equal value ""))
  && String.for_all (function '0' .. '9' -> true | _ -> false) value

let decimal_int64_of_string_opt value =
  if is_decimal value then int64_of_string_opt value else None

let non_negative_int_of_string_opt value =
  match int_of_string_opt value with
  | Some value when Int.compare value 0 >= 0 -> Some value
  | _ -> None

let non_negative_int64_of_string_opt value =
  match int64_of_string_opt value with
  | Some value when Int64.compare value 0L >= 0 -> Some value
  | _ -> None
