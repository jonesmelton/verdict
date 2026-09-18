open Verdict

let ok = Test_common.ok

let rejected name = function
  | Error _ -> ()
  | Ok _ -> Alcotest.fail name
;;

let accepted name = function
  | Ok _ -> ()
  | Error e -> Alcotest.failf "%s: %s" name (Error.message e)
;;

let tests () =
  let getenv = function
    | "TYPESAFE_API_KEY" -> Some " token "
    | "TYPESAFE_DEFAULT_MODEL" -> Some " "
    | _ -> None
  in
  let config =
    match Config.create ~getenv () with
    | Ok config -> config
    | Error e -> Alcotest.fail (Error.message e)
  in
  Alcotest.(check string) "trimmed key" "token" (Config.api_key config);
  Alcotest.(check string) "default model" "jev-latest" (Config.model config);
  Alcotest.(check string)
    "blank model falls back"
    "jev-latest"
    (Config.model (ok (Config.create ~api_key:"k" ~model:"   " ())));
  Alcotest.(check string)
    "model is used"
    "custom"
    (Config.model (ok (Config.create ~api_key:"k" ~model:"custom" ())));
  Alcotest.(check (float 0.0)) "timeout" 10.0 (Config.timeout config);
  List.iter
    (fun base_url ->
       rejected ("reject " ^ base_url) (Config.create ~api_key:"x" ~base_url ()))
    [ "http://example.com"
    ; "http://[::ffff:10.0.0.1]:8080"
    ; "http://10.0.0.1:8080"
    ; "http://[fd00::1]:8080"
    ; "http://[::2]:8080"
    ; "http://[fe80::1]:8080"
    ; "https://user:pass@example.com"
    ; "https://example.com?q=x"
    ; "https://example.com#x"
    ; "file:///tmp/key"
    ; "https://"
    ; "https://not a host"
    ; "https://example.com:0"
    ; "https://example.com:65536"
    ; "https://foo_bar.example"
    ; "https://a_b.com"
    ; "https://-lead.com"
    ; "https://lead-.com"
    ; "https://1.2.3.4.5"
    ; "https://" ^ String.make 64 'a' ^ ".com"
    ];
  List.iter
    (fun timeout -> rejected "bad timeout" (Config.create ~api_key:"x" ~timeout ()))
    [ nan; infinity; 0.0; -1.0 ];
  rejected "header injection" (Config.create ~api_key:"x\r\ny" ());
  rejected "missing key" (Config.create ~getenv:(fun _ -> None) ());
  rejected "body bound" (Config.create ~api_key:"x" ~max_response_bytes:0 ());
  List.iter
    (fun base_url -> accepted base_url (Config.create ~api_key:"x" ~base_url ()))
    [ "http://127.0.0.1:8080/prefix/"
    ; "http://127.0.0.1:8080/"
    ; "http://[::1]:8080"
    ; "http://[::ffff:127.0.0.1]:8080"
    ; "http://localhost"
    ; "http://127.0.0.2:8080"
    ; "https://api.typesafe.ai"
    ; "https://api.typesafe.ai/v1/"
    ; "https://sub.api.typesafe.ai"
    ; "https://example.com."
    ]
;;

let () =
  Alcotest.run
    "config"
    [ "validation", [ Alcotest.test_case "configuration" `Quick tests ] ]
;;
