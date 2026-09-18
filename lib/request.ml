type t =
  { model : string option
  ; state : Content.t
  ; questions : Question.packed list
  }

let create ?model ~state questions =
  let ids = List.map (fun (Question.Pack q) -> Question.id q) questions in
  if questions = []
  then Error (Error.Configuration "at least one question is required")
  else if List.length ids <> List.length (List.sort_uniq String.compare ids)
  then Error (Error.Configuration "duplicate question id")
  else Ok { model; state; questions }
;;

let model t = t.model
let questions t = t.questions

let to_yojson t ~model =
  `Assoc
    [ "model", `String model
    ; "state", Content.to_yojson t.state
    ; ( "questions"
      , `Assoc
          (List.map
             (fun (Question.Pack q) -> Question.id q, Question.to_yojson q)
             t.questions) )
    ]
;;
