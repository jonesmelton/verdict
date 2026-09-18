open Verdict

let ok = Test_common.ok

let expect_configuration name r =
  match r with
  | Ok _ -> Alcotest.failf "expected Error for %s" name
  | Error e ->
    Alcotest.(check bool)
      (name ^ " is a configuration error")
      true
      (match e with
       | Error.Configuration _ -> true
       | _ -> false)
;;

let policy ?max_retries ?backoff_initial ?backoff_max ?jitter ?retry_after_cap () =
  ok (Retry.create ?max_retries ?backoff_initial ?backoff_max ?jitter ?retry_after_cap ())
;;

let h l = Cohttp.Header.of_list l

let delay ?(random = 0.0) ?(now = 0.0) t attempt headers =
  Retry.delay t ~attempt ~random ~now ~headers:(h headers)
;;

let check_delay name expected actual = Alcotest.(check (float 1e-9)) name expected actual
let e_1994_0849_00 = 784111740.0
let e_2015_0727_00 = 1445412420.0
let e_2020 = 1577836800.0

let jitter_property () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make
       ~count:500
       ~name:"jitter bounds"
       QCheck.(quad (int_range 1 40) (int_range 0 100) (int_range 0 8) (int_range 0 100))
       (fun (initial, jitter, attempt, random) ->
          let t =
            ok
              (Retry.create
                 ~backoff_initial:(float_of_int initial /. 10.0)
                 ~backoff_max:5.0
                 ~jitter:(float_of_int jitter /. 100.0)
                 ())
          in
          let sampled = delay ~random:(float_of_int random /. 100.0) t attempt [] in
          let unjittered = delay t attempt [] in
          let low = unjittered *. (1.0 -. (float_of_int jitter /. 100.0)) in
          Float.is_finite sampled
          && sampled >= 0.0
          && sampled >= low -. 0.002
          && sampled <= unjittered +. 0.002)
;;

let retry_after_property () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make
       ~count:200
       ~name:"retry-after precedence"
       QCheck.(triple (int_range 0 200) (int_range 1 100) (int_range 0 8))
       (fun (cap, requested, attempt) ->
          let t =
            ok
              (Retry.create
                 ~backoff_initial:0.5
                 ~jitter:0.25
                 ~retry_after_cap:(float_of_int cap)
                 ())
          in
          let delay =
            Retry.delay
              t
              ~attempt
              ~random:0.5
              ~now:0.0
              ~headers:(h [ "retry-after", string_of_int requested ])
          in
          delay = Float.min (float_of_int cap) (float_of_int requested))
;;

let () =
  Alcotest.run
    "test_retry"
    [ ( "create"
      , [ Alcotest.test_case "defaults" `Quick (fun () ->
            Alcotest.(check int) "default max_retries" 2 (Retry.max_retries Retry.default))
        ; Alcotest.test_case "create default equals default" `Quick (fun () ->
            Alcotest.(check int)
              "max_retries"
              (Retry.max_retries Retry.default)
              (Retry.max_retries (policy ())))
        ; Alcotest.test_case "max_retries accessor" `Quick (fun () ->
            Alcotest.(check int) "zero" 0 (Retry.max_retries (policy ~max_retries:0 ()));
            Alcotest.(check int) "five" 5 (Retry.max_retries (policy ~max_retries:5 ())))
        ; Alcotest.test_case "negative max_retries" `Quick (fun () ->
            expect_configuration "max_retries" (Retry.create ~max_retries:(-1) ()))
        ; Alcotest.test_case "invalid backoff_initial" `Quick (fun () ->
            List.iter
              (fun v ->
                 expect_configuration
                   "backoff_initial"
                   (Retry.create ~backoff_initial:v ()))
              [ -1.0; Float.nan; Float.infinity ])
        ; Alcotest.test_case "invalid backoff_max" `Quick (fun () ->
            List.iter
              (fun v ->
                 expect_configuration "backoff_max" (Retry.create ~backoff_max:v ()))
              [ -1.0; Float.nan; Float.infinity ])
        ; Alcotest.test_case "invalid jitter" `Quick (fun () ->
            List.iter
              (fun v -> expect_configuration "jitter" (Retry.create ~jitter:v ()))
              [ -0.1; 1.1; Float.nan; Float.infinity ])
        ; Alcotest.test_case "invalid retry_after_cap" `Quick (fun () ->
            List.iter
              (fun v ->
                 expect_configuration
                   "retry_after_cap"
                   (Retry.create ~retry_after_cap:v ()))
              [ -1.0; Float.nan; Float.infinity ])
        ; Alcotest.test_case "boundary values accepted" `Quick (fun () ->
            ignore (policy ~jitter:0.0 ());
            ignore (policy ~jitter:1.0 ());
            ignore (policy ~backoff_initial:0.0 ~backoff_max:0.0 ());
            ignore (policy ~retry_after_cap:0.0 ()))
        ; Alcotest.test_case "validation errors are not retryable" `Quick (fun () ->
            match Retry.create ~max_retries:(-1) () with
            | Ok _ -> Alcotest.fail "expected Error"
            | Error e -> Alcotest.(check bool) "not retryable" false (Error.retryable e))
        ] )
    ; ( "backoff"
      , [ Alcotest.test_case "exponential without jitter" `Quick (fun () ->
            let t = policy ~jitter:0.0 () in
            check_delay "attempt 0" 0.5 (delay t 0 []);
            check_delay "attempt 1" 1.0 (delay t 1 []);
            check_delay "attempt 2" 2.0 (delay t 2 []);
            check_delay "attempt 3" 4.0 (delay t 3 []);
            check_delay "attempt 4 capped" 5.0 (delay t 4 []);
            check_delay "attempt 5 capped" 5.0 (delay t 5 []))
        ; Alcotest.test_case "overflow capped and finite" `Quick (fun () ->
            let t = policy ~jitter:0.0 () in
            let d = delay t 1_000_000 [] in
            check_delay "huge attempt" 5.0 d;
            Alcotest.(check bool) "finite" true (Float.is_finite d))
        ; Alcotest.test_case "zero backoff disables delay" `Quick (fun () ->
            check_delay "initial zero" 0.0 (delay (policy ~backoff_initial:0.0 ()) 3 []);
            check_delay "max zero" 0.0 (delay (policy ~backoff_max:0.0 ()) 0 []);
            check_delay
              "both zero"
              0.0
              (delay (policy ~backoff_initial:0.0 ~backoff_max:0.0 ()) 7 []))
        ; Alcotest.test_case "initial above max clamps to max" `Quick (fun () ->
            check_delay
              "attempt 0"
              5.0
              (delay (policy ~backoff_initial:10.0 ~backoff_max:5.0 ~jitter:0.0 ()) 0 []))
        ; Alcotest.test_case "rounds to milliseconds" `Quick (fun () ->
            let t = policy ~jitter:0.25 () in
            check_delay "one third random" 0.917 (delay ~random:(1.0 /. 3.0) t 1 []))
        ] )
    ; ( "jitter"
      , [ Alcotest.test_case "subtracts a fraction" `Quick (fun () ->
            check_delay "random zero keeps full" 0.5 (delay (policy ~jitter:0.25 ()) 0 []);
            check_delay
              "half jitter half random"
              0.75
              (delay ~random:0.5 (policy ~jitter:0.5 ()) 1 []);
            check_delay
              "full jitter random one"
              0.0
              (delay ~random:1.0 (policy ~jitter:1.0 ()) 0 []);
            check_delay
              "full jitter random zero"
              0.5
              (delay ~random:0.0 (policy ~jitter:1.0 ()) 0 []))
        ; Alcotest.test_case "random is clamped" `Quick (fun () ->
            check_delay "above one" 0.0 (delay ~random:2.0 (policy ~jitter:1.0 ()) 0 []);
            check_delay
              "below zero"
              0.5
              (delay ~random:(-1.0) (policy ~jitter:1.0 ()) 0 []);
            check_delay
              "nan random"
              0.5
              (delay ~random:Float.nan (policy ~jitter:1.0 ()) 0 []))
        ] )
    ; ( "properties"
      , [ Alcotest.test_case "jitter band" `Quick jitter_property
        ; Alcotest.test_case "retry-after precedence" `Quick retry_after_property
        ] )
    ; ( "retry_after"
      , [ Alcotest.test_case "delta seconds" `Quick (fun () ->
            check_delay "integer" 2.0 (delay (policy ()) 0 [ "retry-after", "2" ]);
            check_delay "fraction" 1.5 (delay (policy ()) 0 [ "retry-after", "1.5" ]);
            check_delay "zero" 0.0 (delay (policy ()) 0 [ "retry-after", "0" ]);
            check_delay "whitespace" 3.0 (delay (policy ()) 0 [ "retry-after", "  3  " ]))
        ; Alcotest.test_case "delta milliseconds" `Quick (fun () ->
            check_delay
              "milliseconds"
              0.25
              (delay (policy ()) 0 [ "retry-after-ms", "250" ]);
            check_delay
              "fractional milliseconds"
              0.0015
              (delay (policy ()) 0 [ "retry-after-ms", "1.5" ]))
        ; Alcotest.test_case "milliseconds take precedence" `Quick (fun () ->
            check_delay
              "ms wins"
              0.25
              (delay (policy ()) 0 [ "retry-after-ms", "250"; "retry-after", "30" ]))
        ; Alcotest.test_case "empty values are zero" `Quick (fun () ->
            check_delay
              "empty retry-after"
              0.0
              (delay (policy ()) 0 [ "retry-after", "" ]);
            check_delay
              "empty retry-after-ms"
              0.0
              (delay (policy ()) 0 [ "retry-after-ms", "" ]))
        ; Alcotest.test_case "empty milliseconds wins over seconds" `Quick (fun () ->
            check_delay
              "empty ms"
              0.0
              (delay (policy ()) 0 [ "retry-after-ms", ""; "retry-after", "2" ]))
        ; Alcotest.test_case "nan values" `Quick (fun () ->
            check_delay
              "nan seconds fall back"
              0.5
              (delay (policy ()) 0 [ "retry-after", "nan" ]);
            check_delay
              "nan ms falls through to seconds"
              2.0
              (delay (policy ()) 0 [ "retry-after-ms", "nan"; "retry-after", "2" ]);
            check_delay
              "nan ms alone falls back"
              0.5
              (delay (policy ()) 0 [ "retry-after-ms", "nan" ]))
        ; Alcotest.test_case "infinite values" `Quick (fun () ->
            check_delay
              "inf seconds fall back"
              0.5
              (delay (policy ()) 0 [ "retry-after", "inf" ]);
            check_delay
              "infinity seconds fall back"
              0.5
              (delay (policy ()) 0 [ "retry-after", "1e400" ]);
            check_delay
              "inf ms alone falls back"
              0.5
              (delay (policy ()) 0 [ "retry-after-ms", "inf" ]);
            check_delay
              "inf ms falls through to seconds"
              3.0
              (delay (policy ()) 0 [ "retry-after-ms", "inf"; "retry-after", "3" ]))
        ; Alcotest.test_case "overflow is capped" `Quick (fun () ->
            check_delay
              "huge seconds"
              120.0
              (delay (policy ()) 0 [ "retry-after", "1e300" ]);
            check_delay
              "huge milliseconds"
              120.0
              (delay (policy ()) 0 [ "retry-after-ms", "1e300" ]))
        ; Alcotest.test_case "negative values" `Quick (fun () ->
            check_delay
              "negative seconds fall back"
              0.5
              (delay (policy ()) 0 [ "retry-after", "-1" ]);
            check_delay
              "negative ms alone falls back"
              0.5
              (delay (policy ()) 0 [ "retry-after-ms", "-1" ]);
            check_delay
              "negative ms falls through to seconds"
              2.0
              (delay (policy ()) 0 [ "retry-after-ms", "-1"; "retry-after", "2" ]))
        ; Alcotest.test_case "cap" `Quick (fun () ->
            check_delay
              "default cap"
              120.0
              (delay (policy ()) 0 [ "retry-after", "100000" ]);
            check_delay
              "custom cap"
              5.0
              (delay (policy ~retry_after_cap:5.0 ()) 0 [ "retry-after", "100" ]);
            check_delay
              "zero cap"
              0.0
              (delay (policy ~retry_after_cap:0.0 ()) 0 [ "retry-after", "100" ]))
        ; Alcotest.test_case "numeric values ignore now" `Quick (fun () ->
            check_delay
              "nan now"
              2.0
              (delay ~now:Float.nan (policy ()) 0 [ "retry-after", "2" ]);
            check_delay
              "infinite now"
              2.0
              (delay ~now:Float.infinity (policy ()) 0 [ "retry-after", "2" ]))
        ] )
    ; ( "http_date"
      , [ Alcotest.test_case "imf date" `Quick (fun () ->
            check_delay
              "future"
              37.0
              (delay
                 ~now:e_1994_0849_00
                 (policy ())
                 0
                 [ "retry-after", "Sun, 06 Nov 1994 08:49:37 GMT" ]);
            check_delay
              "past"
              0.0
              (delay
                 ~now:e_1994_0849_00
                 (policy ())
                 0
                 [ "retry-after", "Sun, 06 Nov 1994 08:48:00 GMT" ]);
            check_delay
              "equal"
              0.0
              (delay
                 ~now:784111777.0
                 (policy ())
                 0
                 [ "retry-after", "Sun, 06 Nov 1994 08:49:37 GMT" ]))
        ; Alcotest.test_case "rfc850 two digit year" `Quick (fun () ->
            check_delay
              "nineteen hundreds"
              37.0
              (delay
                 ~now:e_1994_0849_00
                 (policy ())
                 0
                 [ "retry-after", "Sunday, 06-Nov-94 08:49:37 GMT" ]);
            check_delay
              "past nineteen sixty nine"
              0.0
              (delay
                 ~now:e_2020
                 (policy ())
                 0
                 [ "retry-after", "Thursday, 06-Nov-69 08:49:37 GMT" ]);
            check_delay
              "future two thousands"
              120.0
              (delay
                 ~now:e_2020
                 (policy ())
                 0
                 [ "retry-after", "Tuesday, 06-Nov-68 08:49:37 GMT" ]))
        ; Alcotest.test_case "asctime date" `Quick (fun () ->
            check_delay
              "future"
              37.0
              (delay
                 ~now:e_1994_0849_00
                 (policy ())
                 0
                 [ "retry-after", "Sun Nov  6 08:49:37 1994" ]))
        ; Alcotest.test_case "overflow date is capped" `Quick (fun () ->
            check_delay
              "year 9999"
              120.0
              (delay
                 ~now:e_2020
                 (policy ())
                 0
                 [ "retry-after", "Fri, 31 Dec 9999 23:59:59 GMT" ]))
        ; Alcotest.test_case "date respects explicit cap" `Quick (fun () ->
            check_delay
              "cap ten"
              10.0
              (delay
                 ~now:e_2015_0727_00
                 (policy ~retry_after_cap:10.0 ())
                 0
                 [ "retry-after", "Wed, 21 Oct 2015 07:28:00 GMT" ]))
        ; Alcotest.test_case "invalid dates fall back to backoff" `Quick (fun () ->
            let t = policy () in
            List.iter
              (fun v -> check_delay ("invalid " ^ v) 0.5 (delay t 0 [ "retry-after", v ]))
              [ "not a date"
              ; "Wed, 32 Nov 2015 08:49:37 GMT"
              ; "Mon, 30 Feb 2015 08:49:37 GMT"
              ; "Wed, 21 Xxx 2015 07:28:00 GMT"
              ])
        ; Alcotest.test_case "invalid now falls back to backoff" `Quick (fun () ->
            let t = policy () in
            check_delay
              "nan now"
              0.5
              (delay
                 ~now:Float.nan
                 t
                 0
                 [ "retry-after", "Wed, 21 Oct 2015 07:28:00 GMT" ]);
            check_delay
              "infinite now"
              0.5
              (delay
                 ~now:Float.infinity
                 t
                 0
                 [ "retry-after", "Wed, 21 Oct 2015 07:28:00 GMT" ]))
        ] )
    ]
;;
