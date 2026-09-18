let ( let* ) = Result.bind

let evaluate ~sw ~net ~clock =
  let open Verdict in
  let* config = Config.create ~timeout:10. () in
  let* client = Client.create ~net ~clock config in
  let* spam =
    Question.noul
      ~id:"spam"
      ~instructions:(Content.text "Is this message unsolicited advertising?")
      ()
  in
  let* tone =
    Question.choice
      ~id:"tone"
      ~instructions:(Content.text "What is the tone of this message?")
      [ "angry", Some (Content.text "Upset or hostile")
      ; "calm", Some (Content.text "Neutral or polite")
      ; "excited", None
      ]
      ()
  in
  let* request =
    Request.create
      ~state:(Content.text "I was charged twice for the same subscription. Please help.")
      [ Question.pack spam; Question.pack tone ]
  in
  let* responses = Client.evaluate client ~sw request in
  let usage = Response.usage responses in
  Printf.printf
    "model=%s request_id=%s input_tokens=%d output_tokens=%d\n"
    (Response.model responses)
    (Response.request_id responses |> Option.value ~default:"-")
    usage.Usage.input_tokens
    usage.Usage.output_tokens;
  (match Response.find responses spam with
   | Some answer ->
     Printf.printf "spam probability = %.3f\n" (Probability.to_float answer.probability)
   | None -> print_endline "spam: no answer");
  (match Response.find responses tone with
   | Some answer ->
     Printf.printf
       "tone = %s (confidence %.3f)\n"
       answer.choice
       (Confidence.to_float answer.confidence)
   | None -> print_endline "tone: no answer");
  Ok ()
;;

let () =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      match evaluate ~sw ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) with
      | Ok () -> ()
      | Error e ->
        prerr_endline (Verdict.Error.message e);
        exit 1))
;;
