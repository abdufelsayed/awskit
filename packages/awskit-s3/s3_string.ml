let is_prefix ~prefix value =
  let prefix_len = String.length prefix in
  String.length value >= prefix_len
  && String.equal (String.sub value 0 prefix_len) prefix

let is_suffix ~suffix value =
  let suffix_len = String.length suffix in
  let len = String.length value in
  len >= suffix_len
  && String.equal (String.sub value (len - suffix_len) suffix_len) suffix

let substring_equal value start expected =
  let len = String.length expected in
  String.length value >= start + len
  && String.equal (String.sub value start len) expected

let has_ctl_or_del value =
  String.exists
    (fun c ->
      let code = Char.code c in
      code < 0x20 || Int.equal code 0x7F)
    value
