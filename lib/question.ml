type _ kind =
  | Noul : Answer.Noul.t kind
  | Choice : Answer.Choice.t kind
  | Score : Answer.Score.t kind

type 'a t =
  { id : string
  ; kind : 'a kind
  ; json : Yojson.Safe.t
  }

type packed = Pack : 'a t -> packed

let id t = t.id
let pack t = Pack t
let to_yojson t = t.json

let nullable = function
  | None -> `Null
  | Some c -> Content.to_yojson c
;;

let make ~id ~kind ~tag ~instructions fields =
  if String.trim id = ""
  then Error (Error.Configuration "question id must not be blank")
  else
    Ok
      { id
      ; kind
      ; json =
          `Assoc
            (("type", `String tag) :: ("instructions", nullable instructions) :: fields)
      }
;;

let noul ~id ?instructions ?yes ?no () =
  let criteria = `Assoc [ "true", nullable yes; "false", nullable no ] in
  make ~id ~kind:Noul ~tag:"noul" ~instructions [ "criteria", criteria ]
;;

let choice ~id ?instructions criteria () =
  let n = List.length criteria in
  let names = List.map fst criteria in
  if n < 1 || n > 255
  then Error (Error.Configuration "choice requires 1–255 options")
  else if List.exists (fun s -> String.trim s = "") names
  then Error (Error.Configuration "choice names must not be blank")
  else if List.length (List.sort_uniq String.compare names) <> n
  then Error (Error.Configuration "duplicate choice name")
  else
    make
      ~id
      ~kind:Choice
      ~tag:"choice"
      ~instructions
      [ "criteria", `Assoc (List.map (fun (k, v) -> k, nullable v) criteria) ]
;;

let score ~id ?instructions criteria () =
  let n = List.length criteria in
  if n < 2 || n > 10
  then Error (Error.Configuration "score requires 2–10 levels")
  else if List.exists Content.is_blank criteria
  then Error (Error.Configuration "score levels must not be empty")
  else
    make
      ~id
      ~kind:Score
      ~tag:"score"
      ~instructions
      [ "criteria", `List (List.map Content.to_yojson criteria) ]
;;

let kind_name : type a. a t -> string =
  fun t ->
  match t.kind with
  | Noul -> "noul"
  | Choice -> "choice"
  | Score -> "score"
;;

let select : type a. a t -> Answer.t -> a option =
  fun t answer ->
  match t.kind, answer with
  | Noul, Answer.Noul a -> Some a
  | Choice, Answer.Choice a -> Some a
  | Score, Answer.Score a -> Some a
  | _ -> None
;;
