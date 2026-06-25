type scenario = {
  name : string;
  request_method : Awskit.Request.Method.t;
  response_status : int;
  response_headers : (string * string) list;
  response_bytes : string;
  keep_connection_open : bool;
  expected_body : string;
}

let content_length_5 = [ ("Content-Length", "5") ]

let scenarios =
  [
    {
      name = "HEAD 200 with advertised body is empty";
      request_method = `HEAD;
      response_status = 200;
      response_headers = content_length_5;
      response_bytes = "";
      keep_connection_open = true;
      expected_body = "";
    };
    {
      name = "HEAD 404 with advertised body is empty";
      request_method = `HEAD;
      response_status = 404;
      response_headers = content_length_5;
      response_bytes = "";
      keep_connection_open = true;
      expected_body = "";
    };
    {
      name = "GET 204 with advertised body is empty";
      request_method = `GET;
      response_status = 204;
      response_headers = content_length_5;
      response_bytes = "";
      keep_connection_open = true;
      expected_body = "";
    };
    {
      name = "GET 304 with advertised body is empty";
      request_method = `GET;
      response_status = 304;
      response_headers = content_length_5;
      response_bytes = "";
      keep_connection_open = true;
      expected_body = "";
    };
    {
      name = "GET 200 chunked body stops at terminating chunk";
      request_method = `GET;
      response_status = 200;
      response_headers = [ ("Transfer-Encoding", "chunked") ];
      response_bytes = "5\r\nhello\r\n0\r\n\r\n";
      keep_connection_open = true;
      expected_body = "hello";
    };
  ]

let bodiless_scenarios =
  List.filter (fun scenario -> String.equal scenario.expected_body "") scenarios

let chunked_scenarios =
  List.filter
    (fun scenario -> not (String.equal scenario.expected_body ""))
    scenarios
