open Verdict

let getenv name =
  match Sys.getenv_opt name with
  | Some s -> s
  | None -> Alcotest.failf "%s is not set" name
;;

let ok = function
  | Ok x -> x
  | Error e -> Alcotest.failf "%s" (Error.message e)
;;

let client env =
  let config =
    ok
      (Config.create
         ~api_key:(getenv "TYPESAFE_API_KEY")
         ?base_url:(Sys.getenv_opt "TYPESAFE_BASE_URL")
         ?model:(Sys.getenv_opt "TYPESAFE_DEFAULT_MODEL")
         ~timeout:30.
         ~retry:(ok (Retry.create ~max_retries:0 ()))
         ())
  in
  ok (Client.create ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) config)
;;

let live_models () =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let models = ok (Client.list_models (client env) ~sw ()) in
      Alcotest.(check bool) "non-empty" true (models <> []);
      List.iter
        (fun (m : Model.t) ->
           Alcotest.(check bool) "name" true (String.trim m.name <> "");
           Alcotest.(check bool) "description" true (String.trim m.description <> "");
           print_endline
             (Printf.sprintf "%s (%s): %s" m.name m.release_date m.description))
        models))
;;

let live_evaluate () =
  let billing =
    ok
      (Question.noul
         ~id:"billing"
         ~instructions:(Content.text "Is this message about billing?")
         ())
  in
  let urgency =
    ok
      (Question.score
         ~id:"urgency"
         ~instructions:(Content.text "How urgent is it?")
         [ Content.text "Can wait"; Content.text "This week"; Content.text "Today" ]
         ())
  in
  let request =
    ok
      (Request.create
         ~state:
           (Content.text "I was charged twice for the same subscription. Please help.")
         [ Question.pack billing; Question.pack urgency ])
  in
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let responses = ok (Client.evaluate (client env) ~sw request) in
      let id = Option.value ~default:"<none>" (Response.request_id responses) in
      print_endline
        (Printf.sprintf "request_id=%s model=%s" id (Response.model responses));
      (match Response.find responses billing with
       | Some answer ->
         let p = Probability.to_float answer.probability in
         Alcotest.(check bool) "probability in range" true (p >= 0. && p <= 1.);
         print_endline (Printf.sprintf "billing=%.3f" p)
       | None -> Alcotest.fail "missing billing answer");
      (match Response.find responses urgency with
       | Some answer ->
         print_endline
           (Printf.sprintf
              "urgency=%.3f (confidence=%.3f)"
              answer.score
              (Confidence.to_float answer.confidence))
       | None -> Alcotest.fail "missing urgency answer");
      let usage = Response.usage responses in
      Alcotest.(check bool) "input tokens" true (usage.Usage.input_tokens > 0);
      Alcotest.(check bool) "output tokens" true (usage.Usage.output_tokens > 0)))
;;

let () =
  Alcotest.run
    "live"
    [ ( "live"
      , [ Alcotest.test_case "GET /v1/models" `Quick live_models
        ; Alcotest.test_case "POST /v1/systemone" `Quick live_evaluate
        ] )
    ]
;;
