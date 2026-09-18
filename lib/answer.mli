(** The answer shapes the API returns, one per question kind.

    Answer records are [private], not opaque: callers read fields
    ([answer.probability]), pattern-match on values, and pass them around
    freely, but cannot build a record or use [{ answer with ... }]. Every answer
    originates on the wire, so construction funnels through the [make]
    functions below, whose parameters are already-validated scalar types. *)

(** The map type used for choice probabilities and for the raw answer table. *)
module String_map : Map.S with type key = string

(** The map type used for score legends and probabilities, keyed by rubric
    level. *)
module Int_map : Map.S with type key = int

(** A noul answer. The deployed [NoulAnswer] schema defines this as a single
    probability and nothing else: there is no boolean and no confidence field,
    so this type has neither. *)
module Noul : sig
  type t = private { probability : Probability.t }

  val make : probability:Probability.t -> t
end

(** A choice answer: the selected label, the confidence in the answer as a
    whole, and the distribution over the offered labels, keyed by label. *)
module Choice : sig
  type t = private
    { choice : string
    ; confidence : Confidence.t
    ; probabilities : Probability.t String_map.t
    }

  val make
    :  choice:string
    -> confidence:Confidence.t
    -> probabilities:Probability.t String_map.t
    -> t

  (** [probability t label] is [None] when [label] is absent from the
      distribution. *)
  val probability : t -> string -> Probability.t option
end

(** A score answer.

    [score] is the probability-weighted average of the rubric levels as defined
    by the deployed schema. It is a plain [float] and is {e not} a probability:
    it may lie between levels ([1.7]), and it is not bounded to \[0,1\], which
    is why it is not a {!Probability.t}.

    Levels are keyed by non-negative integers because the API encodes rubric
    levels as stringified integers. Decoding rejects non-canonical spellings
    such as ["01"] or ["-1"] instead of re-normalising them. *)
module Score : sig
  type t = private
    { score : float
    ; confidence : Confidence.t
    ; legend : Yojson.Safe.t Int_map.t
    ; probabilities : Probability.t Int_map.t
    }

  val make
    :  score:float
    -> confidence:Confidence.t
    -> legend:Yojson.Safe.t Int_map.t
    -> probabilities:Probability.t Int_map.t
    -> t

  (** [legend_text t level] is [Some text] only when the legend entry for
      [level] is a JSON string, and [None] when the level is absent or its entry
      has another shape. *)
  val legend_text : t -> int -> string option

  (** [probability t level] is [None] when [level] is absent from the
      distribution. *)
  val probability : t -> int -> Probability.t option
end

type t =
  | Noul of Noul.t
  | Choice of Choice.t
  | Score of Score.t

(** The wire discriminator: ["noul"], ["choice"], or ["score"]. *)
val type_name : t -> string
