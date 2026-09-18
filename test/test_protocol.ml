open Verdict

let ok = Test_common.ok
let text = Content.text

let fixture =
  {|{"model":"jev-test","answers":{"urgent":{"type":"noul","noul":0.98},"tone":{"type":"choice","choice":"calm","confidence":0.8,"probabilities":{"calm":0.8,"angry":0.19}},"priority":{"type":"score","score":0.9,"confidence":0.7,"legend":{"0":"low","1":{"label":"high"}},"probabilities":{"0":0.1,"1":0.9}},"future":{"type":"future","data":42}},"usage":{"input_tokens":12,"output_tokens":3}}|}
;;

let urgent = ok (Question.noul ~id:"urgent" ~instructions:(text "Urgent?") ())

let tone =
  ok (Question.choice ~id:"tone" [ "calm", None; "angry", Some (text "angry") ] ())
;;

let priority = ok (Question.score ~id:"priority" [ text "low"; text "high" ] ())

let request () =
  ok
    (Request.create
       ~model:"jev-test"
       ~state:(text "hello")
       [ Question.pack urgent; Question.pack tone; Question.pack priority ])
;;

let test_encode () =
  let json = Request.to_yojson ~model:"jev-test" (request ()) in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "discriminator"
    "noul"
    (json |> member "questions" |> member "urgent" |> member "type" |> to_string);
  Alcotest.(check string) "state" "hello" (json |> member "state" |> to_string);
  Alcotest.(check bool)
    "nullable choice criterion"
    true
    (json
     |> member "questions"
     |> member "tone"
     |> member "criteria"
     |> member "calm"
     = `Null)
;;

let test_decode () =
  let response = ok (Response.of_yojson (Yojson.Safe.from_string fixture)) in
  let answer = Option.get (Response.find response urgent) in
  Alcotest.(check (float 0.00001))
    "typed noul"
    0.98
    (Probability.to_float answer.probability);
  Alcotest.(check string)
    "typed choice"
    "calm"
    (Option.get (Response.find response tone)).choice;
  let score = Option.get (Response.find response priority) in
  Alcotest.(check (option string))
    "string legend"
    (Some "low")
    (Answer.Score.legend_text score 0);
  Alcotest.(check (option string))
    "structured legend"
    None
    (Answer.Score.legend_text score 1);
  Alcotest.(check bool)
    "unknown remains raw"
    true
    (Option.is_some (Response.raw_answer response "future"));
  Alcotest.(check int) "usage" 12 (Response.usage response).Usage.input_tokens;
  let wrong = ok (Question.choice ~id:"urgent" [ "yes", None ] ()) in
  Alcotest.(check bool)
    "wrong tag cannot cast"
    true
    (Option.is_none (Response.find response wrong))
;;

let test_validation () =
  let bad label r = Alcotest.(check bool) label true (Result.is_error r) in
  bad "score minimum" (Question.score ~id:"s" [ text "one" ] ());
  bad "score maximum" (Question.score ~id:"s" (List.init 11 (fun _ -> text "level")) ());
  bad "empty choices" (Question.choice ~id:"c" [] ());
  bad "duplicate choices" (Question.choice ~id:"c" [ "x", None; "x", None ] ());
  bad "blank criterion" (Question.score ~id:"s" [ text " "; text "ok" ] ());
  bad "empty request" (Request.create ~model:"m" ~state:(text "") []);
  bad
    "duplicate id"
    (Request.create
       ~model:"m"
       ~state:(text "")
       [ Question.pack urgent; Question.pack urgent ]);
  bad "scalar content" (Content.of_yojson (`Int 1));
  bad "nonfinite nested JSON" (Content.of_yojson (`Assoc [ "x", `Float nan ]))
;;

let test_paths () =
  let json =
    Yojson.Safe.from_string
      {|{"model":"m","answers":{"x":{"type":"choice","choice":"a","confidence":"bad","probabilities":{}}},"usage":{"input_tokens":1,"output_tokens":2}}|}
  in
  match Response.of_yojson json with
  | Error (Error.Decode { path; _ }) ->
    Alcotest.(check string) "field path" "answers.x.confidence" path
  | _ -> Alcotest.fail "expected decode error"
;;

let test_content_property () =
  QCheck.Test.check_exn
    (QCheck.Test.make ~count:200 QCheck.string (fun s ->
       match Content.of_yojson (Content.to_yojson (text s)) with
       | Ok c -> Content.to_yojson c = `String s
       | Error _ -> false))
;;

let () =
  Alcotest.run
    "protocol"
    [ ( "wire"
      , List.map
          (fun (n, f) -> Alcotest.test_case n `Quick f)
          [ "encoding", test_encode
          ; "typed decoding", test_decode
          ; "validation", test_validation
          ; "decode paths", test_paths
          ; "content property", test_content_property
          ] )
    ]
;;
