open Verdict

let contains = Test_common.contains
let ok = Test_common.ok

let describe result =
  Test_common.describe
    (fun (body, _) -> "ok (" ^ String.sub body 0 (min 40 (String.length body)) ^ "...)")
    result
;;

let with_server handler f =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let net = Eio.Stdenv.net env in
      let socket =
        Eio.Net.listen
          ~sw
          ~reuse_addr:true
          ~backlog:8
          net
          (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
      in
      let port =
        match Eio.Net.listening_addr socket with
        | `Tcp (_, p) -> p
        | _ -> assert false
      in
      let stop, resolve = Eio.Promise.create () in
      let server = Cohttp_eio.Server.make ~callback:handler () in
      Eio.Fiber.fork ~sw (fun () ->
        Cohttp_eio.Server.run ~stop ~on_error:(fun ex -> raise ex) socket server);
      Fun.protect
        ~finally:(fun () -> Eio.Promise.resolve resolve ())
        (fun () -> f env sw port (Printf.sprintf "http://127.0.0.1:%d/prefix" port))))
;;

let respond ?(headers = Cohttp.Header.init ()) status body =
  Cohttp_eio.Server.respond ~headers ~status ~body:(Eio.Flow.string_source body) ()
;;

let transport env base_url ?timeout ?max_response_bytes ?retry () =
  let config =
    ok
      (Config.create ?timeout ?max_response_bytes ?retry ~api_key:"test-key" ~base_url ())
  in
  ok (Transport.make ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) config)
;;

let headers_retry () =
  let count = ref 0 in
  let handler _ req body =
    incr count;
    let h = Cohttp.Request.headers req in
    Alcotest.(check (option string))
      "auth"
      (Some "Bearer test-key")
      (Cohttp.Header.get h "authorization");
    Alcotest.(check (option string))
      "retry count header"
      (if !count = 1 then None else Some "1")
      (Cohttp.Header.get h "x-typesafe-retry-count");
    Alcotest.(check (option string))
      "no transparent content encoding"
      (Some "identity")
      (Cohttp.Header.get h "accept-encoding");
    List.iter
      (fun key ->
         Alcotest.(check bool) key true (Option.is_some (Cohttp.Header.get h key)))
      [ "accept"; "content-type"; "user-agent"; "x-typesafe-sdk"; "x-typesafe-runtime" ];
    Alcotest.(check string)
      "path prefix"
      "/prefix/v1/systemone"
      (Uri.path (Cohttp.Request.uri req));
    Alcotest.(check string)
      "body"
      "{}"
      (Eio.Buf_read.(parse_exn take_all) body ~max_size:100);
    if !count = 1 then respond (`Code 529) "overloaded" else respond `OK "{}"
  in
  with_server handler (fun env sw _port base_url ->
    let retry = ok (Retry.create ~jitter:0.0 ~backoff_initial:0.0 ()) in
    let t = transport env base_url ~retry () in
    ignore
      (ok (Transport.request t ~sw ~meth:`POST ~path:"/v1/systemone" ~body:(Some "{}")));
    Alcotest.(check int) "attempts" 2 !count)
;;

let error_text_excludes_credentials () =
  let handler _ _req _body =
    respond
      ~headers:(Cohttp.Header.of_list [ "x-typesafe-request-id", "req-42" ])
      (`Code 500)
      "{\"error\":{\"message\":\"upstream exploded\"},\"api_key\":\"sk-secret\"}"
  in
  with_server handler (fun env sw _port base_url ->
    let retry = ok (Retry.create ~jitter:0.0 ~backoff_initial:0.0 ~max_retries:0 ()) in
    let t = transport env base_url ~retry () in
    match Transport.request t ~sw ~meth:`POST ~path:"/v1/systemone" ~body:(Some "{}") with
    | Error (Error.Api { message; request_id; body; _ } as e) ->
      let rendered = Error.message e in
      List.iter
        (fun (what, secret) ->
           Alcotest.(check bool) what false (contains rendered secret))
        [ "no credential in message", "sk-secret"
        ; "no configured key in message", "test-key"
        ];
      Alcotest.(check bool) "body is preserved" true (contains body "sk-secret");
      Alcotest.(check string) "message" "upstream exploded" message;
      Alcotest.(check (option string)) "request id" (Some "req-42") request_id
    | other -> Alcotest.failf "expected an API error, got %s" (describe other))
;;

let status_and_bound () =
  List.iter
    (fun (status, body, max_response_bytes, expected) ->
       let count = ref 0 in
       with_server
         (fun _ _ _ ->
            incr count;
            respond status body)
         (fun env sw _port base_url ->
            let t = transport env base_url ~max_response_bytes () in
            let result =
              Transport.request t ~sw ~meth:`GET ~path:"/v1/models" ~body:None
            in
            Alcotest.(check bool) (describe result) true (expected result);
            Alcotest.(check int) "no retry" 1 !count))
    [ ( `Unauthorized
      , "unauthorized"
      , 100
      , function
        | Error (Error.Api { status; _ }) -> status = 401
        | _ -> false )
    ; ( `OK
      , String.make 101 'a'
      , 100
      , function
        | Error (Error.Response_too_large 100) -> true
        | _ -> false )
    ; ( `Found
      , "redirect"
      , 100
      , function
        | Error (Error.Api { status; _ }) -> status = 302
        | _ -> false )
    ]
;;

exception Cancelled_by_caller

type counters =
  { accepts : int ref
  ; closed : int ref
  }

let with_raw_server ~handler f =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let net = Eio.Stdenv.net env in
      let clock = Eio.Stdenv.clock env in
      let socket =
        Eio.Net.listen
          ~sw
          ~reuse_addr:true
          ~backlog:8
          net
          (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
      in
      let port =
        match Eio.Net.listening_addr socket with
        | `Tcp (_, p) -> p
        | _ -> assert false
      in
      let counters = { accepts = ref 0; closed = ref 0 } in
      Eio.Fiber.fork_daemon ~sw (fun () ->
        while true do
          Eio.Switch.run (fun connection_sw ->
            let flow, _peer = Eio.Net.accept ~sw:connection_sw socket in
            incr counters.accepts;
            handler flow;
            try
              let bytes = Cstruct.create 1024 in
              while true do
                ignore (Eio.Flow.single_read flow bytes)
              done
            with
            | End_of_file | Eio.Io _ -> incr counters.closed)
        done;
        `Stop_daemon);
      f env sw clock port counters))
;;

let settled ~clock counters n =
  let start = Eio.Time.now clock in
  while !(counters.closed) < n && Eio.Time.now clock -. start < 5.0 do
    Eio.Time.sleep clock 0.01
  done;
  !(counters.closed)
;;

let per_attempt_teardown () =
  with_raw_server
    ~handler:(fun _ -> ())
    (fun env sw clock port counters ->
       let retry =
         ok (Retry.create ~jitter:0.0 ~max_retries:2 ~backoff_initial:0.01 ())
       in
       let t =
         transport env (Printf.sprintf "http://127.0.0.1:%d" port) ~timeout:0.05 ~retry ()
       in
       (match Transport.request t ~sw ~meth:`GET ~path:"/v1/models" ~body:None with
        | Error (Error.Timeout 0.05) -> ()
        | other -> Alcotest.failf "expected a timeout, got %s" (describe other));
       Alcotest.(check int) "one connection per attempt" 3 !(counters.accepts);
       Alcotest.(check int) "no connection leaked" 3 (settled ~clock counters 3))
;;

let caller_cancellation_stops_retrying () =
  with_raw_server
    ~handler:(fun _ -> ())
    (fun env _sw clock port counters ->
       let retry =
         ok (Retry.create ~jitter:0.0 ~max_retries:5 ~backoff_initial:0.05 ())
       in
       let t =
         transport env (Printf.sprintf "http://127.0.0.1:%d" port) ~timeout:5.0 ~retry ()
       in
       let outcome =
         try
           Eio.Switch.run (fun call_sw ->
             Eio.Fiber.fork ~sw:call_sw (fun () ->
               Eio.Time.sleep clock 0.05;
               Eio.Switch.fail call_sw Cancelled_by_caller);
             ignore
               (Transport.request t ~sw:call_sw ~meth:`GET ~path:"/v1/models" ~body:None);
             `returned)
         with
         | Cancelled_by_caller -> `cancelled
         | Eio.Cancel.Cancelled _ -> `cancelled
         | ex -> `other (Printexc.to_string ex)
       in
       (match outcome with
        | `cancelled -> ()
        | `returned -> Alcotest.fail "cancellation was swallowed"
        | `other reason -> Alcotest.failf "unexpected exception: %s" reason);
       Alcotest.(check int) "no further attempts after cancellation" 1 !(counters.accepts);
       Alcotest.(check int)
         "cancelled attempt closed its socket"
         1
         (settled ~clock counters 1))
;;

let tls_failure_is_not_retried () =
  let cleartext = "HTTP/1.1 200 OK\r\ncontent-length: 2\r\n\r\n{}" in
  with_raw_server
    ~handler:(fun flow -> Eio.Flow.copy_string cleartext flow)
    (fun env sw _clock port counters ->
       let t =
         transport env (Printf.sprintf "https://127.0.0.1:%d" port) ~timeout:5.0 ()
       in
       (match Transport.request t ~sw ~meth:`GET ~path:"/v1/models" ~body:None with
        | Error (Error.Tls reason) when reason <> "" -> ()
        | other -> Alcotest.failf "expected a TLS failure, got %s" (describe other));
       Alcotest.(check int) "handshake failure is terminal" 1 !(counters.accepts))
;;

let https_transport_builds () =
  Eio_main.run (fun env ->
    let config = ok (Config.create ~api_key:"k" ~base_url:"https://api.typesafe.ai" ()) in
    ignore
      (ok (Transport.make ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) config)))
;;

let truncated_body_is_a_connection_error () =
  with_raw_server
    ~handler:(fun flow ->
      Eio.Flow.copy_string
        "HTTP/1.1 200 OK\r\ncontent-length: 100\r\n\r\n{\"answers\":"
        flow;
      Eio.Flow.shutdown flow `Send)
    (fun env sw clock port counters ->
       let retry = ok (Retry.create ~jitter:0.0 ~max_retries:1 ~backoff_initial:0.0 ()) in
       let t =
         transport env (Printf.sprintf "http://127.0.0.1:%d" port) ~timeout:2.0 ~retry ()
       in
       (match
          Transport.request t ~sw ~meth:`POST ~path:"/v1/systemone" ~body:(Some "{}")
        with
        | Error (Error.Connection reason) ->
          Alcotest.(check string)
            "truncation is reported"
            "response body truncated after 11 of 100 bytes"
            reason
        | other -> Alcotest.failf "expected a connection error, got %s" (describe other));
       Alcotest.(check int) "truncated responses are retried" 2 !(counters.accepts);
       Alcotest.(check int) "no connection leaked" 2 (settled ~clock counters 2))
;;

let connection_closed_before_response_is_a_connection_error () =
  with_raw_server
    ~handler:(fun flow -> Eio.Flow.shutdown flow `Send)
    (fun env sw _clock port counters ->
       let retry = ok (Retry.create ~jitter:0.0 ~max_retries:0 ()) in
       let t =
         transport env (Printf.sprintf "http://127.0.0.1:%d" port) ~timeout:2.0 ~retry ()
       in
       (match Transport.request t ~sw ~meth:`GET ~path:"/v1/models" ~body:None with
        | Error (Error.Connection reason) ->
          Alcotest.(check string)
            "cohttp-eio end-of-file message is classified"
            "connection closed by peer"
            reason
        | other -> Alcotest.failf "expected a connection error, got %s" (describe other));
       Alcotest.(check int) "one attempt" 1 !(counters.accepts))
;;

let cancellation_interrupts_backoff () =
  with_raw_server
    ~handler:(fun flow ->
      Eio.Flow.copy_string
        "HTTP/1.1 429 Too Many Requests\r\nretry-after: 30\r\ncontent-length: 0\r\n\r\n"
        flow)
    (fun env _sw clock port counters ->
       let t =
         transport
           env
           (Printf.sprintf "http://127.0.0.1:%d" port)
           ~timeout:5.0
           ~retry:(ok (Retry.create ~jitter:0.0 ~max_retries:3 ()))
           ()
       in
       let started = Eio.Time.now clock in
       let elapsed, outcome =
         try
           Eio.Switch.run (fun call_sw ->
             Eio.Fiber.fork ~sw:call_sw (fun () ->
               Eio.Time.sleep clock 0.1;
               Eio.Switch.fail call_sw Cancelled_by_caller);
             ignore
               (Transport.request t ~sw:call_sw ~meth:`GET ~path:"/v1/models" ~body:None));
           Eio.Time.now clock -. started, `returned
         with
         | Cancelled_by_caller | Eio.Cancel.Cancelled _ ->
           Eio.Time.now clock -. started, `cancelled
         | ex -> Eio.Time.now clock -. started, `other (Printexc.to_string ex)
       in
       (match outcome with
        | `cancelled -> ()
        | `returned -> Alcotest.fail "cancellation was swallowed"
        | `other reason -> Alcotest.failf "unexpected exception: %s" reason);
       Alcotest.(check bool)
         (Printf.sprintf "cancelled after %.2fs of a 30s wait" elapsed)
         true
         (elapsed < 2.0);
       Alcotest.(check int) "one attempt before cancellation" 1 !(counters.accepts))
;;

let () =
  Alcotest.run
    "transport"
    [ ( "HTTP"
      , [ Alcotest.test_case "headers and 529 retry" `Quick headers_retry
        ; Alcotest.test_case "status and body limit" `Quick status_and_bound
        ; Alcotest.test_case "per-attempt teardown" `Quick per_attempt_teardown
        ; Alcotest.test_case
            "caller cancellation"
            `Quick
            caller_cancellation_stops_retrying
        ; Alcotest.test_case "credential redaction" `Quick error_text_excludes_credentials
        ; Alcotest.test_case "truncated body" `Quick truncated_body_is_a_connection_error
        ; Alcotest.test_case
            "closed before response"
            `Quick
            connection_closed_before_response_is_a_connection_error
        ; Alcotest.test_case
            "cancellation interrupts backoff"
            `Quick
            cancellation_interrupts_backoff
        ; Alcotest.test_case "TLS failure not retried" `Quick tls_failure_is_not_retried
        ; Alcotest.test_case "TLS client config builds" `Quick https_transport_builds
        ] )
    ]
;;
