open Verdict

let contains = Test_common.contains

type request_record =
  { meth : string
  ; path : string
  ; body : string
  ; headers : Cohttp.Header.t
  }

let ( |. ) json key =
  match json, key with
  | `Assoc xs, k ->
    (match List.assoc_opt k xs with
     | Some v -> v
     | None -> `Null)
  | _ -> `Null
;;

let string_field key json =
  match json |. key with
  | `String s -> s
  | other -> Alcotest.failf "%s is not a string: %s" key (Yojson.Safe.to_string other)
;;

let ok = Test_common.ok
let describe result = Test_common.describe (fun _ -> "a successful response") result

let with_server ?max_response_bytes reply f =
  let seen = ref [] in
  let handler _ req body =
    let text = Eio.Buf_read.(parse_exn take_all) body ~max_size:100_000 in
    let record =
      { meth = Cohttp.Code.string_of_method (Cohttp.Request.meth req)
      ; path = Uri.path (Cohttp.Request.uri req)
      ; body = text
      ; headers = Cohttp.Request.headers req
      }
    in
    seen := record :: !seen;
    let status, headers, payload = reply (List.length !seen) record in
    Cohttp_eio.Server.respond
      ~headers:(Cohttp.Header.of_list headers)
      ~status
      ~body:(Eio.Flow.string_source payload)
      ()
  in
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
        (fun () ->
           let config =
             ok
               (Config.create
                  ?max_response_bytes
                  ~api_key:"test-key"
                  ~base_url:(Printf.sprintf "http://127.0.0.1:%d" port)
                  ~model:"jev-latest"
                  ~retry:(ok (Retry.create ~jitter:0.0 ~backoff_initial:0.0 ()))
                  ())
           in
           let client = ok (Client.create ~net ~clock:(Eio.Stdenv.clock env) config) in
           let result = f client sw in
           result, List.rev !seen)))
;;

let noul_answer =
  {|{"model":"jev-2.5-pro","answers":{"spam":{"type":"noul","noul":0.02},"billing":{"type":"noul","noul":0.97}},"usage":{"input_tokens":120,"output_tokens":12}}|}
;;

let evaluation_request () =
  let spam = ok (Question.noul ~id:"spam" ()) in
  let billing =
    ok
      (Question.noul
         ~id:"billing"
         ~instructions:(Content.text "Is this about billing?")
         ())
  in
  let request =
    ok
      (Request.create
         ~state:(Content.text "I was charged twice. Please help.")
         [ Question.pack spam; Question.pack billing ])
  in
  request, spam, billing
;;

let evaluate_and_retry () =
  let request, spam, billing = evaluation_request () in
  let responses, seen =
    with_server
      (fun count record ->
         Alcotest.(check string) "path" "/v1/systemone" record.path;
         Alcotest.(check string) "method" "POST" record.meth;
         if count = 1
         then `Code 529, [ "retry-after-ms", "0" ], "overloaded"
         else (
           Alcotest.(check (option string))
             "retry count header"
             (Some "1")
             (Cohttp.Header.get record.headers "x-typesafe-retry-count");
           `OK, [ "x-typesafe-request-id", "req-1" ], noul_answer))
      (fun client sw -> ok (Client.evaluate client ~sw request))
  in
  Alcotest.(check int) "attempts" 2 (List.length seen);
  Alcotest.(check (option string))
    "request id"
    (Some "req-1")
    (Response.request_id responses);
  Alcotest.(check string)
    "server picks the model"
    "jev-2.5-pro"
    (Response.model responses);
  let usage = Response.usage responses in
  Alcotest.(check int) "input tokens" 120 usage.Usage.input_tokens;
  Alcotest.(check int) "output tokens" 12 usage.Usage.output_tokens;
  (match Response.find responses spam with
   | Some answer ->
     Alcotest.(check (float 1e-9))
       "spam probability"
       0.02
       (Probability.to_float answer.probability)
   | None -> Alcotest.fail "missing spam answer");
  match Response.find responses billing with
  | Some answer ->
    Alcotest.(check (float 1e-9))
      "billing probability"
      0.97
      (Probability.to_float answer.probability)
  | None -> Alcotest.fail "missing billing answer"
;;

let request_encoding_and_model_override () =
  let tone =
    ok
      (Question.choice
         ~id:"tone"
         ~instructions:(Content.text "What is the tone?")
         [ "angry", Some (Content.text "Hostile or impatient"); "calm", None ]
         ())
  in
  let urgency =
    ok (Question.score ~id:"urgency" [ Content.text "Can wait"; Content.text "Today" ] ())
  in
  let request =
    ok
      (Request.create
         ~state:(ok (Content.of_yojson (`Assoc [ "subject", `String "hi" ])))
         [ Question.pack tone; Question.pack urgency ])
  in
  let payload =
    {|{"model":"jev-latest","answers":{"tone":{"type":"choice","choice":"calm","confidence":0.5,"probabilities":{"angry":0.5,"calm":0.5}},"urgency":{"type":"score","score":1.0, "confidence":0.75,"legend":{"0":"Can wait","1":"Today"},"probabilities":{"0":0.25,"1":0.75}}},"usage":{"input_tokens":1,"output_tokens":1}}|}
  in
  let (), seen =
    with_server
      (fun _ _ -> `OK, [], payload)
      (fun client sw ->
         let overridden = ok (Client.evaluate client ~sw ~model:"jev-exact" request) in
         let defaulted = ok (Client.evaluate client ~sw request) in
         (match Response.find overridden tone with
          | Some answer -> Alcotest.(check string) "selected choice" "calm" answer.choice
          | None -> Alcotest.fail "missing tone answer");
         match Response.find defaulted urgency with
         | Some answer ->
           Alcotest.(check (float 1e-9))
             "score probability"
             0.75
             (Option.map Probability.to_float (Answer.Score.probability answer 1)
              |> Option.value ~default:0.)
         | None -> Alcotest.fail "missing urgency answer")
  in
  Alcotest.(check int) "requests" 2 (List.length seen);
  let body i = Yojson.Safe.from_string (List.nth seen i).body in
  Alcotest.(check string) "override wins" "jev-exact" (string_field "model" (body 0));
  Alcotest.(check string) "config default" "jev-latest" (string_field "model" (body 1));
  let questions = body 0 |. "questions" in
  Alcotest.(check string)
    "choice tag"
    "choice"
    (string_field "type" (questions |. "tone"));
  Alcotest.(check bool)
    "criteria travel"
    true
    (questions |. "urgency" |. "criteria" <> `Null)
;;

let answers_are_matched_by_identity () =
  let request, spam, billing = evaluation_request () in
  let absent = ok (Question.noul ~id:"absent" ()) in
  let (), _ =
    with_server
      (fun _ _ -> `OK, [ "x-typesafe-request-id", "req-9" ], noul_answer)
      (fun client sw ->
         let responses = ok (Client.evaluate client ~sw request) in
         Alcotest.(check bool) "first handle" true (Response.find responses spam <> None);
         Alcotest.(check bool)
           "second handle"
           true
           (Response.find responses billing <> None);
         match
           Client.evaluate
             client
             ~sw
             (ok (Request.create ~state:(Content.text "x") [ Question.pack absent ]))
         with
         | Error (Error.Decode { path; request_id; _ }) ->
           Alcotest.(check string) "path" "answers.absent" path;
           Alcotest.(check (option string)) "id" (Some "req-9") request_id
         | other -> Alcotest.failf "expected a decode error, got %s" (describe other))
  in
  ()
;;

let malformed_body () =
  let request, _, _ = evaluation_request () in
  let (), _ =
    with_server
      (fun _ _ -> `OK, [ "x-typesafe-request-id", "req-10" ], "<html>gateway</html>")
      (fun client sw ->
         match Client.evaluate client ~sw request with
         | Error (Error.Decode { message; request_id; _ }) ->
           Alcotest.(check string) "message" "malformed JSON" message;
           Alcotest.(check (option string)) "id" (Some "req-10") request_id
         | other -> Alcotest.failf "expected a decode error, got %s" (describe other))
  in
  ()
;;

let api_error_surfaces_status_and_id () =
  let request, _, _ = evaluation_request () in
  let (), seen =
    with_server
      (fun _ _ ->
         ( `Unauthorized
         , [ "x-typesafe-request-id", "req-11" ]
         , {|{"detail":[{"loc":["body","model"],"msg":"unknown model","type":"value_error"}]}|}
         ))
      (fun client sw ->
         match Client.evaluate client ~sw request with
         | Error (Error.Api { status; message; request_id; body }) ->
           Alcotest.(check int) "status" 401 status;
           Alcotest.(check string) "message" "model: unknown model" message;
           Alcotest.(check (option string)) "id" (Some "req-11") request_id;
           Alcotest.(check bool) "body kept" true (String.length body > 10)
         | other -> Alcotest.failf "expected an API error, got %s" (describe other))
  in
  Alcotest.(check int) "not retried" 1 (List.length seen)
;;

let list_models_decodes () =
  let payload =
    {|{"models":[{"name":"jev-latest","description":"General-purpose system one model.","release_date":"2026-09-15"},{"name":"jev-exact","description":"More deterministic.","release_date":"2026-04-01"}]}|}
  in
  let models, seen =
    with_server
      (fun _ record ->
         Alcotest.(check string) "method" "GET" record.meth;
         Alcotest.(check string) "path" "/v1/models" record.path;
         Alcotest.(check string) "no body" "" record.body;
         `OK, [], payload)
      (fun client sw -> ok (Client.list_models client ~sw ()))
  in
  Alcotest.(check int) "requests" 1 (List.length seen);
  Alcotest.(check (list string))
    "names"
    [ "jev-latest"; "jev-exact" ]
    (List.map (fun (m : Model.t) -> m.name) models);
  Alcotest.(check (list string))
    "release dates"
    [ "2026-09-15"; "2026-04-01" ]
    (List.map (fun (m : Model.t) -> m.release_date) models)
;;

let oversized_response_is_not_retried () =
  let request, _, _ = evaluation_request () in
  let (), seen =
    with_server
      ~max_response_bytes:40
      (fun _ _ -> `OK, [], noul_answer)
      (fun client sw ->
         match Client.evaluate client ~sw request with
         | Error (Error.Response_too_large 40) -> ()
         | other -> Alcotest.failf "expected a size error, got %s" (describe other))
  in
  Alcotest.(check int) "single attempt" 1 (List.length seen)
;;

let answer_kinds_are_enforced () =
  let choice = ok (Question.choice ~id:"spam" [ "yes", None; "no", None ] ()) in
  let request =
    ok (Request.create ~state:(Content.text "hello") [ Question.pack choice ])
  in
  let rejected ~path ~about json =
    ignore
    @@ with_server
         (fun _ _ -> `OK, [], json)
         (fun client sw ->
            match Client.evaluate client ~sw request with
            | Error (Error.Decode { path = got; message; _ }) ->
              Alcotest.(check string) "path" path got;
              Alcotest.(check bool)
                (Printf.sprintf "%s explains %s" path about)
                true
                (String.length message > 0 && contains message about)
            | other -> Alcotest.failf "expected a decode error, got %s" (describe other))
  in
  rejected
    ~path:"answers.spam"
    ~about:"choice"
    {|{"model":"m","answers":{"spam":{"type":"noul","noul":0.5}},"usage":{"input_tokens":1,"output_tokens":1}}|};
  rejected
    ~path:"answers.spam.type"
    ~about:"string"
    {|{"model":"m","answers":{"spam":{"type":5}},"usage":{"input_tokens":1,"output_tokens":1}}|};
  rejected
    ~path:"answers.spam.type"
    ~about:"missing"
    {|{"model":"m","answers":{"spam":{"noul":0.5}},"usage":{"input_tokens":1,"output_tokens":1}}|}
;;

let unknown_answer_types_are_tolerated () =
  let spam = ok (Question.noul ~id:"spam" ()) in
  let request =
    ok (Request.create ~state:(Content.text "hello") [ Question.pack spam ])
  in
  let (), _ =
    with_server
      (fun _ _ ->
         ( `OK
         , []
         , {|{"model":"m","answers":{"spam":{"type":"noul","noul":0.5},"future":{"type":"quantum","q":[1,2]}},"usage":{"input_tokens":1,"output_tokens":1}}|}
         ))
      (fun client sw ->
         let responses = ok (Client.evaluate client ~sw request) in
         Alcotest.(check bool)
           "asked answer decodes"
           true
           (Response.find responses spam <> None);
         Alcotest.(check bool)
           "unknown answer stays raw"
           true
           (Response.raw_answer responses "future" <> None))
  in
  ()
;;

let () =
  Alcotest.run
    "client"
    [ ( "client"
      , [ Alcotest.test_case "evaluate with retry" `Quick evaluate_and_retry
        ; Alcotest.test_case "request encoding" `Quick request_encoding_and_model_override
        ; Alcotest.test_case
            "answers are matched by identity"
            `Quick
            answers_are_matched_by_identity
        ; Alcotest.test_case "decode errors carry request ids" `Quick malformed_body
        ; Alcotest.test_case "api errors" `Quick api_error_surfaces_status_and_id
        ; Alcotest.test_case "list models" `Quick list_models_decodes
        ; Alcotest.test_case "answer kinds are enforced" `Quick answer_kinds_are_enforced
        ; Alcotest.test_case
            "unknown answer types are tolerated"
            `Quick
            unknown_answer_types_are_tolerated
        ; Alcotest.test_case
            "response size limit"
            `Quick
            oversized_response_is_not_retried
        ] )
    ]
;;
