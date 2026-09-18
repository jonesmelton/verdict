open Verdict

let contains = Test_common.contains

let headers ?request_id () =
  match request_id with
  | None -> Cohttp.Header.init ()
  | Some id -> Cohttp.Header.of_list [ "x-typesafe-request-id", id ]
;;

let sanitized id =
  Error.request_id (Cohttp.Header.of_list [ "x-typesafe-request-id", id ])
;;

let of_response ?request_id ~status body =
  Error.of_response ~status ~headers:(headers ?request_id ()) ~body
;;

let api_message = function
  | Error.Api { message; _ } -> message
  | _ -> Alcotest.fail "expected Api"
;;

let api_status = function
  | Error.Api { status; _ } -> status
  | _ -> Alcotest.fail "expected Api"
;;

let api_body = function
  | Error.Api { body; _ } -> body
  | _ -> Alcotest.fail "expected Api"
;;

let api_request_id = function
  | Error.Api { request_id; _ } -> request_id
  | _ -> Alcotest.fail "expected Api"
;;

let extraction_cases =
  [ "error as string", {|{"error":"boom"}|}, "boom"
  ; ( "error as object with message"
    , {|{"error":{"message":"nested error"}}|}
    , "nested error" )
  ; ( "error string wins over message and detail"
    , {|{"error":"e","message":"m","detail":"d"}|}
    , "e" )
  ; "message wins over detail", {|{"message":"m","detail":"d"}|}, "m"
  ; "message string", {|{"message":"plain message"}|}, "plain message"
  ; "detail string", {|{"detail":"detail string"}|}, "detail string"
  ; ( "detail object with message"
    , {|{"detail":{"message":"detail nested"}}|}
    , "detail nested" )
  ; ( "detail validation array"
    , {|{"detail":[{"loc":["body","questions","q","criteria"],"msg":"Field required"}]}|}
    , "questions.q.criteria: Field required" )
  ; ( "detail validation array drops body prefix"
    , {|{"detail":[{"loc":["body","state"],"msg":"Field required"}]}|}
    , "state: Field required" )
  ; ( "detail validation array with integer index"
    , {|{"detail":[{"loc":["body","questions",0,"score"],"msg":"Input should be a valid number"}]}|}
    , "questions.0.score: Input should be a valid number" )
  ; ( "detail validation array joins entries"
    , {|{"detail":[{"loc":["body","x"],"msg":"a"},{"loc":["body","y"],"msg":"b"}]}|}
    , "x: a; y: b" )
  ; ( "detail validation entry without loc"
    , {|{"detail":[{"msg":"just a message"}]}|}
    , "just a message" )
  ; ( "detail validation array skips entries without msg"
    , {|{"detail":[{"loc":["body","x"],"msg":"a"},{"loc":["body","y"]},{"msg":123},{"not":"a dict"}]}|}
    , "x: a" )
  ; "json string body", {|"raw string"|}, "raw string"
  ; "non json body is not a message", "<html>Internal Server Error</html>", ""
  ; "empty body", "", ""
  ; "unknown json object has no message", {|{"foo":"bar"}|}, ""
  ; "json array has no message", {|["a","b"]|}, ""
  ; "json null has no message", "null", ""
  ; ( "error object without string message falls through"
    , {|{"error":{"message":123},"message":"top"}|}
    , "top" )
  ; ( "detail array without usable entries falls through"
    , {|{"detail":[{"loc":["body"],"msg":123}]}|}
    , "" )
  ; "error empty string is not used", {|{"error":"","message":"fallback"}|}, "fallback"
  ]
;;

let () =
  Alcotest.run
    "test_error"
    [ ( "message"
      , [ Alcotest.test_case "configuration" `Quick (fun () ->
            Alcotest.(check string)
              "configuration"
              "bad configuration"
              (Error.message (Error.Configuration "bad configuration")))
        ; Alcotest.test_case "timeout" `Quick (fun () ->
            let m = Error.message (Error.Timeout 10.0) in
            Alcotest.(check bool) "mentions timeout" true (contains m "timed out");
            Alcotest.(check bool) "mentions value" true (contains m "10"))
        ; Alcotest.test_case "connection" `Quick (fun () ->
            Alcotest.(check string)
              "connection"
              "connection reset by peer"
              (Error.message (Error.Connection "connection reset by peer")))
        ; Alcotest.test_case "response too large" `Quick (fun () ->
            let m = Error.message (Error.Response_too_large 1048576) in
            Alcotest.(check bool) "mentions size" true (contains m "1048576"))
        ; Alcotest.test_case "decode" `Quick (fun () ->
            let m =
              Error.message
                (Error.Decode
                   { path = "answers.tone.confidence"
                   ; message = "missing field"
                   ; request_id = None
                   })
            in
            Alcotest.(check bool)
              "mentions path"
              true
              (contains m "answers.tone.confidence");
            Alcotest.(check bool) "mentions message" true (contains m "missing field"))
        ; Alcotest.test_case "api with extracted message" `Quick (fun () ->
            let e = of_response ~status:503 {|{"message":"temporarily unavailable"}|} in
            Alcotest.(check int) "status" 503 (api_status e);
            Alcotest.(check string)
              "body preserved"
              {|{"message":"temporarily unavailable"}|}
              (api_body e);
            let m = Error.message e in
            Alcotest.(check bool) "mentions status" true (contains m "503");
            Alcotest.(check bool)
              "mentions message"
              true
              (contains m "temporarily unavailable"))
        ; Alcotest.test_case "api without message does not leak body" `Quick (fun () ->
            let e = of_response ~status:500 "<html>Internal Server Error</html>" in
            Alcotest.(check string) "no extracted message" "" (api_message e);
            let m = Error.message e in
            Alcotest.(check bool) "no raw body" false (contains m "Internal Server Error"))
        ; Alcotest.test_case "api with request id" `Quick (fun () ->
            let e =
              of_response ~request_id:"req_123" ~status:429 {|{"message":"slow down"}|}
            in
            Alcotest.(check (option string))
              "request id"
              (Some "req_123")
              (api_request_id e))
        ] )
    ; ( "retryable"
      , [ Alcotest.test_case "timeout" `Quick (fun () ->
            Alcotest.(check bool)
              "timeout retryable"
              true
              (Error.retryable (Error.Timeout 1.0)))
        ; Alcotest.test_case "connection" `Quick (fun () ->
            Alcotest.(check bool)
              "connection retryable"
              true
              (Error.retryable (Error.Connection "reset")))
        ; Alcotest.test_case "configuration" `Quick (fun () ->
            Alcotest.(check bool)
              "configuration not retryable"
              false
              (Error.retryable (Error.Configuration "x")))
        ; Alcotest.test_case "response too large" `Quick (fun () ->
            Alcotest.(check bool)
              "too large not retryable"
              false
              (Error.retryable (Error.Response_too_large 10)))
        ; Alcotest.test_case "decode" `Quick (fun () ->
            Alcotest.(check bool)
              "decode not retryable"
              false
              (Error.retryable
                 (Error.Decode { path = "a"; message = "b"; request_id = None })))
        ]
        @ List.map
            (fun (status, expected) ->
               Alcotest.test_case (Printf.sprintf "status %d" status) `Quick (fun () ->
                 let e = Error.of_response ~status ~headers:(headers ()) ~body:"" in
                 Alcotest.(check bool)
                   (Printf.sprintf "%d retryable" status)
                   expected
                   (Error.retryable e)))
            [ 408, true
            ; 429, true
            ; 500, true
            ; 502, true
            ; 503, true
            ; 504, true
            ; 529, true
            ; 501, true
            ; 599, true
            ; 200, false
            ; 204, false
            ; 302, false
            ; 400, false
            ; 401, false
            ; 403, false
            ; 404, false
            ; 422, false
            ; 499, false
            ] )
    ; ( "of_response"
      , [ Alcotest.test_case "extraction matrix" `Quick (fun () ->
            List.iter
              (fun (name, body, expected) ->
                 Alcotest.(check string)
                   name
                   expected
                   (api_message (of_response ~status:422 body)))
              extraction_cases)
        ; Alcotest.test_case "request id absent" `Quick (fun () ->
            Alcotest.(check (option string))
              "no request id"
              None
              (api_request_id (of_response ~status:500 "")))
        ; Alcotest.test_case "request id header is case insensitive" `Quick (fun () ->
            let e =
              Error.of_response
                ~status:500
                ~headers:(Cohttp.Header.of_list [ "X-TypeSafe-Request-Id", "abc" ])
                ~body:""
            in
            match e with
            | Error.Api { request_id; _ } ->
              Alcotest.(check (option string)) "request id" (Some "abc") request_id
            | _ -> Alcotest.fail "expected Api")
        ; Alcotest.test_case "status and body preserved" `Quick (fun () ->
            let e = of_response ~status:404 {|{"detail":"not found"}|} in
            Alcotest.(check int) "status" 404 (api_status e);
            Alcotest.(check string) "body" {|{"detail":"not found"}|} (api_body e);
            Alcotest.(check string) "message" "not found" (api_message e))
        ; Alcotest.test_case "server text is flattened" `Quick (fun () ->
            let body = "{\"message\":\"first\\nsecond\\tthird\"}" in
            let e =
              Error.of_response
                ~status:500
                ~headers:(Cohttp.Header.of_list [ "x-typesafe-request-id", "abc" ])
                ~body
            in
            match e with
            | Error.Api { message; request_id; body = raw; _ } ->
              Alcotest.(check string)
                "message has no line breaks"
                "first second\tthird"
                message;
              Alcotest.(check string) "body preserved verbatim" body raw;
              Alcotest.(check (option string)) "request id" (Some "abc") request_id;
              let rendered = Error.message e in
              Alcotest.(check bool)
                "rendered error is one line"
                true
                (not (contains rendered "\n"))
            | _ -> Alcotest.fail "expected Api")
        ; Alcotest.test_case "request ids are sanitised" `Quick (fun () ->
            Alcotest.(check (option string))
              "control characters and newlines are flattened"
              (Some "id [31m padding")
              (sanitized "id\u{1b}[31m\r\n  padding");
            Alcotest.(check (option string))
              "blank ids are dropped"
              None
              (sanitized "  \n");
            let clipped = Option.value ~default:"" (sanitized (String.make 400 'a')) in
            Alcotest.(check int) "overlong ids are clipped" 128 (String.length clipped))
        ] )
    ; ( "of_exn"
      , [ Alcotest.test_case "timeout" `Quick (fun () ->
            match Error.of_exn ~timeout:7.5 Eio.Time.Timeout with
            | Error.Timeout 7.5 -> ()
            | other -> Alcotest.failf "expected a timeout, got %s" (Error.message other))
        ; Alcotest.test_case "eof is a connection error" `Quick (fun () ->
            match Error.of_exn ~timeout:1.0 End_of_file with
            | Error.Connection _ -> ()
            | other ->
              Alcotest.failf "expected a connection error, got %s" (Error.message other))
        ; Alcotest.test_case "cohttp framing errors" `Quick (fun () ->
            List.iter
              (fun (msg, expected) ->
                 match Error.of_exn ~timeout:1.0 (Failure msg) with
                 | Error.Connection reason -> Alcotest.(check string) expected msg reason
                 | other -> Alcotest.failf "%s: %s" expected (Error.message other)
                 | exception exn ->
                   Alcotest.failf "%s: raised %s" expected (Printexc.to_string exn))
              [ "connection closed by peer", "framing eof"
              ; "failed to resolve hostname", "resolution"
              ])
        ; Alcotest.test_case "malformed response is a decode error" `Quick (fun () ->
            match
              Error.of_exn ~timeout:1.0 (Failure "Malformed response first line: x")
            with
            | Error.Decode { message; _ } ->
              Alcotest.(check bool)
                "mentions malformation"
                true
                (contains message "malformed HTTP response")
            | other ->
              Alcotest.failf "expected a decode error, got %s" (Error.message other))
        ; Alcotest.test_case "scheme errors are configuration errors" `Quick (fun () ->
            List.iter
              (fun msg ->
                 match Error.of_exn ~timeout:1.0 (Failure msg) with
                 | Error.Configuration _ -> ()
                 | other -> Alcotest.failf "%s: %s" msg (Error.message other))
              [ "no host specified (in x)"
              ; "Unknown scheme ftp"
              ; "HTTPS not enabled (for x)"
              ])
        ; Alcotest.test_case "unknown exceptions propagate" `Quick (fun () ->
            List.iter
              (fun exn ->
                 match
                   try
                     ignore (Error.of_exn ~timeout:1.0 exn);
                     `returned
                   with
                   | ex ->
                     if Printexc.to_string ex = Printexc.to_string exn
                     then `raised
                     else `other
                 with
                 | `raised -> ()
                 | `returned -> Alcotest.failf "%s was swallowed" (Printexc.to_string exn)
                 | `other -> Alcotest.failf "%s was rewritten" (Printexc.to_string exn))
              [ Failure "invariant violated"; Invalid_argument "bytes"; Sys.Break; Exit ])
        ] )
    ]
;;
