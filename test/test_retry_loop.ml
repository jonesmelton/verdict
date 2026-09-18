open Verdict

let ok = Test_common.ok

let phrase = function
  | 200 -> "OK"
  | 401 -> "Unauthorized"
  | 429 -> "Too Many Requests"
  | 500 -> "Internal Server Error"
  | 503 -> "Service Unavailable"
  | 529 -> "Overloaded"
  | _ -> "Status"
;;

let http_text ?(headers = []) status body =
  let extra =
    List.map (fun (k, v) -> Printf.sprintf "%s: %s\r\n" k v) headers |> String.concat ""
  in
  Printf.sprintf
    "HTTP/1.1 %d %s\r\ncontent-length: %d\r\n%s\r\n%s"
    status
    (phrase status)
    (String.length body)
    extra
    body
;;

type outcome =
  { status : int
  ; body : string
  ; headers : (string * string) list
  ; after : float
  }

let respond ?(headers = []) ?(after = 0.0) status body = { status; body; headers; after }
let stalled seconds outcome = { outcome with after = seconds }

let noul id value =
  Printf.sprintf {|{"type":"noul","noul":%g}|} value
  |> fun a ->
  Printf.sprintf
    {|{"model":"jev","answers":{%S:%s},"usage":{"input_tokens":1,"output_tokens":1}}|}
    id
    a
;;

let policy ?jitter ?max_retries ?backoff_initial ?backoff_max ?retry_after_cap () =
  ok (Retry.create ?jitter ?max_retries ?backoff_initial ?backoff_max ?retry_after_cap ())
;;

let with_mock ~timeout ~retry ~outcomes request =
  let attempts = ref 0 in
  Eio_mock.Backend.run_full (fun env ->
    let clock = env#clock in
    Eio.Switch.run (fun sw ->
      let flows =
        List.mapi
          (fun i { status; body; headers; after } ->
             let flow = Eio_mock.Flow.make (Printf.sprintf "socket-%d" i) in
             let payload = http_text ~headers status body in
             Eio_mock.Flow.on_read
               flow
               [ `Run
                   (fun () ->
                     incr attempts;
                     Eio.Time.sleep clock after;
                     payload)
               ];
             flow)
          outcomes
      in
      let net = Eio_mock.Net.make "verdict" in
      Eio_mock.Net.on_getaddrinfo
        net
        (List.map (fun _ -> `Return [ `Tcp (Eio.Net.Ipaddr.V4.loopback, 443) ]) flows);
      Eio_mock.Net.on_connect net (List.map (fun flow -> `Return flow) flows);
      let config =
        ok
          (Config.create
             ~api_key:"mock-key"
             ~base_url:"http://127.0.0.1"
             ~timeout
             ~retry
             ())
      in
      let transport = ok (Transport.make ~net ~clock config) in
      let started = Eio.Time.now clock in
      let result = request transport sw in
      result, Eio.Time.now clock -. started, !attempts))
;;

let get transport sw =
  Transport.request transport ~sw ~meth:`GET ~path:"/v1/models" ~body:None
;;

let check_elapsed name expected elapsed =
  Alcotest.(check (float 1e-6)) name expected elapsed
;;

let test_backoff_sequence () =
  let result, elapsed, attempts =
    with_mock
      ~timeout:10.
      ~retry:(policy ~jitter:0. ~backoff_initial:0.5 ~backoff_max:5.0 ())
      ~outcomes:[ respond 500 "one"; respond 503 "two"; respond 200 (noul "x" 0.5) ]
      get
  in
  ignore (ok result);
  check_elapsed "backoff total" 1.5 elapsed;
  Alcotest.(check int) "attempts" 3 attempts
;;

let test_retry_after_precedence () =
  let result, elapsed, attempts =
    with_mock
      ~timeout:10.
      ~retry:(policy ~jitter:0. ~backoff_initial:0.5 ())
      ~outcomes:
        [ respond
            ~headers:[ "retry-after-ms", "250"; "retry-after", "90" ]
            429
            "slow down"
        ; respond 200 (noul "x" 0.5)
        ]
      get
  in
  ignore (ok result);
  check_elapsed "milliseconds win" 0.25 elapsed;
  Alcotest.(check int) "attempts" 2 attempts
;;

let test_retry_after_capped () =
  let result, elapsed, _ =
    with_mock
      ~timeout:10.
      ~retry:(policy ~retry_after_cap:120. ())
      ~outcomes:
        [ respond ~headers:[ "retry-after", "86400" ] 503 "later"
        ; respond 200 (noul "x" 0.5)
        ]
      get
  in
  ignore (ok result);
  check_elapsed "hostile server cannot block the client" 120. elapsed
;;

let test_retry_after_exceeds_attempt_timeout () =
  let result, elapsed, attempts =
    with_mock
      ~timeout:10.
      ~retry:(policy ~jitter:0. ())
      ~outcomes:
        [ respond ~headers:[ "retry-after", "30" ] 429 "wait"
        ; respond 200 (noul "x" 0.25)
        ]
      get
  in
  ignore (ok result);
  check_elapsed "sleep survives the attempt budget" 30. elapsed;
  Alcotest.(check int) "attempts" 2 attempts
;;

let test_http_date_retry_after () =
  let result, elapsed, _ =
    with_mock
      ~timeout:10.
      ~retry:(policy ~jitter:0. ())
      ~outcomes:
        [ respond
            ~headers:[ "retry-after", "Thu, 01 Jan 1970 00:00:07 GMT" ]
            529
            "overloaded"
        ; respond 200 (noul "x" 0.5)
        ]
      get
  in
  ignore (ok result);
  check_elapsed "date is relative to the mock clock" 7. elapsed
;;

let test_attempt_timeout_is_retried () =
  let result, elapsed, attempts =
    with_mock
      ~timeout:1.
      ~retry:(policy ~jitter:0. ~backoff_initial:0.25 ~max_retries:1 ())
      ~outcomes:[ stalled 3600. (respond 200 "never"); respond 200 (noul "x" 0.75) ]
      get
  in
  ignore (ok result);
  check_elapsed "timeout plus backoff" 1.25 elapsed;
  Alcotest.(check int) "attempts" 2 attempts
;;

let test_retries_exhausted () =
  let result, elapsed, attempts =
    with_mock
      ~timeout:10.
      ~retry:(policy ~jitter:0. ~backoff_initial:0.5 ~max_retries:2 ())
      ~outcomes:[ respond 500 "a"; respond 500 "b"; respond 500 "c" ]
      get
  in
  (match result with
   | Error (Error.Api { status; body; _ }) ->
     Alcotest.(check int) "status" 500 status;
     Alcotest.(check string) "last body retained" "c" body
   | _ -> Alcotest.fail "expected an API error");
  check_elapsed "two delays" 1.5 elapsed;
  Alcotest.(check int) "attempts" 3 attempts
;;

let test_client_error_is_not_retried () =
  let result, elapsed, attempts =
    with_mock
      ~timeout:10.
      ~retry:(policy ~jitter:0. ())
      ~outcomes:[ respond 401 {|{"error":"bad key"}|} ]
      get
  in
  (match result with
   | Error (Error.Api { status; message; _ }) ->
     Alcotest.(check int) "status" 401 status;
     Alcotest.(check string) "message" "bad key" message
   | _ -> Alcotest.fail "expected an API error");
  check_elapsed "no delay" 0. elapsed;
  Alcotest.(check int) "attempts" 1 attempts
;;

let test_zero_retries () =
  let result, elapsed, attempts =
    with_mock
      ~timeout:10.
      ~retry:(policy ~max_retries:0 ())
      ~outcomes:[ respond 529 "busy" ]
      get
  in
  (match result with
   | Error (Error.Api { status; _ }) -> Alcotest.(check int) "status" 529 status
   | _ -> Alcotest.fail "expected an API error");
  check_elapsed "single attempt" 0. elapsed;
  Alcotest.(check int) "attempts" 1 attempts
;;

let () =
  Alcotest.run
    "retry-loop"
    [ ( "eio-mock"
      , List.map
          (fun (name, f) -> Alcotest.test_case name `Quick f)
          [ "exponential backoff", test_backoff_sequence
          ; "retry-after is capped", test_retry_after_capped
          ; "retry-after-ms precedence", test_retry_after_precedence
          ; ( "retry-after exceeds the attempt timeout"
            , test_retry_after_exceeds_attempt_timeout )
          ; "http-date retry-after", test_http_date_retry_after
          ; "attempt timeout retried", test_attempt_timeout_is_retried
          ; "retries exhausted", test_retries_exhausted
          ; "client error not retried", test_client_error_is_not_retried
          ; "zero retries", test_zero_retries
          ] )
    ]
;;
