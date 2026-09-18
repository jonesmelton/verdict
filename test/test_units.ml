open Verdict

let ok = Test_common.ok
let contains = Test_common.contains

let unwrap name = function
  | Ok x -> x
  | Error reason -> Alcotest.failf "%s: %s" name reason
;;

let accepts = function
  | Ok _ -> true
  | Error _ -> false
;;

let test_probability () =
  List.iter
    (fun (name, value, expected) ->
       Alcotest.(check bool)
         ("probability " ^ name)
         expected
         (accepts (Probability.of_float value)))
    [ "zero", 0.0, true
    ; "one", 1.0, true
    ; "half", 0.5, true
    ; "negative", -0.0001, false
    ; "above one", 1.0001, false
    ; "nan", Float.nan, false
    ; "infinity", Float.infinity, false
    ; "negative infinity", Float.neg_infinity, false
    ];
  List.iter
    (fun value ->
       Alcotest.(check (float 0.0))
         (Printf.sprintf "probability %g" value)
         value
         (Probability.to_float (unwrap "probability" (Probability.of_float value))))
    [ 0.0; 0.25; 1.0 ]
;;

let test_confidence () =
  List.iter
    (fun (name, value, expected) ->
       Alcotest.(check bool)
         ("confidence " ^ name)
         expected
         (accepts (Confidence.of_float value)))
    [ "zero", 0.0, true
    ; "one", 1.0, true
    ; "above one", 1.5, false
    ; "negative", -1.0, false
    ; "nan", Float.nan, false
    ]
;;

let test_host () =
  List.iter
    (fun (name, expected) ->
       Alcotest.(check bool) ("host " ^ name) expected (accepts (Host.of_string name)))
    [ "api.typesafe.ai", true
    ; "sub.api.typesafe.ai", true
    ; "localhost", true
    ; "example.com.", true
    ; "127.0.0.1", true
    ; "::1", true
    ; "::ffff:127.0.0.1", true
    ; "foo_bar.example", false
    ; "a_b.com", false
    ; "-lead.com", false
    ; "lead-.com", false
    ; "1.2.3.4.5", false
    ; "not a host", false
    ; "", false
    ; ".", false
    ; String.make 64 'a' ^ ".com", false
    ];
  (match unwrap "ipv4" (Host.of_string "127.0.0.1") with
   | `Ip (Ipaddr.V4 _) -> ()
   | _ -> Alcotest.fail "expected an IPv4 host");
  (match unwrap "ipv6" (Host.of_string "::1") with
   | `Ip (Ipaddr.V6 _) -> ()
   | _ -> Alcotest.fail "expected an IPv6 host");
  match unwrap "name" (Host.of_string "api.typesafe.ai") with
  | `Name _ -> ()
  | _ -> Alcotest.fail "expected a name"
;;

let payload answer =
  Printf.sprintf
    {|{"model":"m","answers":{"q":%s},"usage":{"input_tokens":1,"output_tokens":1}}|}
    answer
;;

let decode_error answer =
  match Response.of_yojson (Yojson.Safe.from_string (payload answer)) with
  | Error (Error.Decode { path; message; _ }) -> path ^ "|" ^ message
  | Error e -> Alcotest.failf "expected a decode error, got %s" (Error.message e)
  | Ok _ -> Alcotest.fail "expected a decode error"
;;

let test_decode_bounds () =
  let reject name needle rendered =
    Alcotest.(check bool) (name ^ " path") true (contains rendered needle)
  in
  reject "noul" "answers.q.noul" (decode_error {|{"type":"noul","noul":-0.1}|});
  reject
    "confidence"
    "answers.q.confidence"
    (decode_error
       {|{"type":"choice","choice":"a","confidence":1.5,"probabilities":{"a":0.5}}|});
  reject
    "probabilities"
    "answers.q.probabilities.a"
    (decode_error
       {|{"type":"choice","choice":"a","confidence":0.5,"probabilities":{"a":2.0}}|});
  List.iter
    (fun answer ->
       ignore (ok (Response.of_yojson (Yojson.Safe.from_string (payload answer)))))
    [ {|{"type":"noul","noul":0.0}|}; {|{"type":"noul","noul":1.0}|} ]
;;

let () =
  Alcotest.run
    "units"
    [ ( "bounds"
      , [ Alcotest.test_case "probability" `Quick test_probability
        ; Alcotest.test_case "confidence" `Quick test_confidence
        ; Alcotest.test_case "host" `Quick test_host
        ; Alcotest.test_case
            "decode rejects out-of-range values"
            `Quick
            test_decode_bounds
        ] )
    ]
;;
