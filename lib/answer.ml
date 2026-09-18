module String_map = Map.Make (String)
module Int_map = Map.Make (Int)

module Noul = struct
  type t = { probability : Probability.t }

  let make ~probability = { probability }
end

module Choice = struct
  type t =
    { choice : string
    ; confidence : Confidence.t
    ; probabilities : Probability.t String_map.t
    }

  let make ~choice ~confidence ~probabilities = { choice; confidence; probabilities }
  let probability t name = String_map.find_opt name t.probabilities
end

module Score = struct
  type t =
    { score : float
    ; confidence : Confidence.t
    ; legend : Yojson.Safe.t Int_map.t
    ; probabilities : Probability.t Int_map.t
    }

  let make ~score ~confidence ~legend ~probabilities =
    { score; confidence; legend; probabilities }
  ;;

  let legend_text t level =
    match Int_map.find_opt level t.legend with
    | Some (`String s) -> Some s
    | _ -> None
  ;;

  let probability t level = Int_map.find_opt level t.probabilities
end

type t =
  | Noul of Noul.t
  | Choice of Choice.t
  | Score of Score.t

let type_name = function
  | Noul _ -> "noul"
  | Choice _ -> "choice"
  | Score _ -> "score"
;;
