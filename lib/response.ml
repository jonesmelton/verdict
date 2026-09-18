module M = Answer.String_map
module I = Answer.Int_map

type t =
  { model : string
  ; usage : Usage.t
  ; answers : Answer.t M.t
  ; raw : Yojson.Safe.t
  ; raw_answers : Yojson.Safe.t M.t
  ; request_id : string option
  }

let src = Logs.Src.create "verdict.codec"

module Log = (val Logs.src_log src : Logs.LOG)

let string_map decode path json =
  Decode.object_ path json
  |> List.fold_left (fun m (k, v) -> M.add k (decode (Decode.at path k) v) m) M.empty
;;

let int_map decode path json =
  Decode.object_ path json
  |> List.fold_left
       (fun m (k, v) ->
          let p = Decode.at path k in
          match int_of_string_opt k with
          | Some i when i >= 0 && string_of_int i = k -> I.add i (decode p v) m
          | _ -> Decode.fail p "expected canonical nonnegative integer key")
       I.empty
;;

let answer id json =
  let open Decode in
  let path = at "answers" id in
  match get string path "type" json with
  | "noul" ->
    Some (Answer.Noul (Answer.Noul.make ~probability:(get probability path "noul" json)))
  | "choice" ->
    Some
      (Answer.Choice
         (Answer.Choice.make
            ~choice:(get string path "choice" json)
            ~confidence:(get confidence path "confidence" json)
            ~probabilities:(get (string_map probability) path "probabilities" json)))
  | "score" ->
    let content path j =
      match Content.of_yojson j with
      | Ok _ -> j
      | Error _ -> fail path "expected string, object, or array"
    in
    Some
      (Answer.Score
         (Answer.Score.make
            ~score:(get number path "score" json)
            ~confidence:(get confidence path "confidence" json)
            ~legend:(get (int_map content) path "legend" json)
            ~probabilities:(get (int_map probability) path "probabilities" json)))
  | tag ->
    Log.warn (fun m ->
      m "Skipped answer %a with unknown type %a" Fmt.string id Fmt.string tag);
    None
;;

let declared_type json =
  match json with
  | `Assoc xs ->
    (match List.assoc_opt "type" xs with
     | Some (`String s) -> s
     | _ -> "malformed")
  | _ -> "malformed"
;;

let check_asked (Question.Pack q) answers raw_answers =
  let name = Question.id q in
  let path = Decode.at "answers" name in
  match M.find_opt name answers with
  | Some answer ->
    if Question.select q answer = None
    then
      Decode.fail
        path
        (Printf.sprintf
           "expected a %s answer, got %s"
           (Question.kind_name q)
           (Answer.type_name answer))
  | None ->
    (match M.find_opt name raw_answers with
     | None -> Decode.fail path "missing answer"
     | Some raw_answer ->
       Decode.fail
         path
         (Printf.sprintf
            "cannot answer a %s question with a %s answer"
            (Question.kind_name q)
            (declared_type raw_answer)))
;;

let of_yojson ?request_id ?asked raw =
  Decode.protect (fun () ->
    let open Decode in
    let model = get string "" "model" raw in
    let u = field "" "usage" raw in
    let usage =
      Usage.
        { input_tokens = get integer "usage" "input_tokens" u
        ; output_tokens = get integer "usage" "output_tokens" u
        }
    in
    let raw_answers = get (string_map (fun _ j -> j)) "" "answers" raw in
    if M.is_empty raw_answers then fail "answers" "expected at least one answer";
    let answers = M.filter_map (fun id j -> answer id j) raw_answers in
    List.iter
      (fun q -> check_asked q answers raw_answers)
      (Option.value ~default:[] asked);
    { model; usage; answers; raw; raw_answers; request_id })
  |> Result.map_error (Error.attach_request_id request_id)
;;

let find t q = Option.bind (M.find_opt (Question.id q) t.answers) (Question.select q)
let raw t = t.raw
let raw_answer t id = M.find_opt id t.raw_answers
let model t = t.model
let usage t = t.usage
let request_id t = t.request_id
