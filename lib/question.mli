(** A question handle indexed by the type of answer it produces: a noul question
    yields {!Answer.Noul.t}, a choice question {!Answer.Choice.t}, and a score
    question {!Answer.Score.t}.

    The API identifies questions by caller-chosen names, so a bare [string id]
    would let a noul answer be read as a score. The index is what prevents that:
    {!select} only recovers an answer of the handle's own type, and the type is
    erased with {!pack} only at the {!Request} boundary, where the name is what
    the server sees. *)

type 'a t
type packed = Pack : 'a t -> packed

(** A question answered by a single probability. [yes] and [no] are the criteria
    text for the two poles; omitting either sends [null] for it.
    [Error.Configuration] if [id] is blank. *)
val noul
  :  id:string
  -> ?instructions:Content.t
  -> ?yes:Content.t
  -> ?no:Content.t
  -> unit
  -> (Answer.Noul.t t, Error.t) result

(** A question answered by choosing one label. Criteria are label-and-optional-
    description pairs. [Error.Configuration] if there are not 1–255 options, if
    any label is blank, or if labels repeat. *)
val choice
  :  id:string
  -> ?instructions:Content.t
  -> (string * Content.t option) list
  -> unit
  -> (Answer.Choice.t t, Error.t) result

(** A question answered on an ordinal rubric, lowest level first, with each
    level's legend text. [Error.Configuration] if there are not 2–10 levels or
    if any level is blank. *)
val score
  :  id:string
  -> ?instructions:Content.t
  -> Content.t list
  -> unit
  -> (Answer.Score.t t, Error.t) result

(** The caller-chosen name the server uses to key this question's answer. *)
val id : 'a t -> string

(** Erase the answer type. Needed to put heterogeneous questions in one
    {!Request}; the type is recovered by {!Response.find} from this handle. *)
val pack : 'a t -> packed

val to_yojson : 'a t -> Yojson.Safe.t

(** ["noul"], ["choice"], or ["score"]. *)
val kind_name : 'a t -> string

(** [select q answer] is [Some a] when [answer] is of [q]'s kind, and [None]
    otherwise. This is the only way to get a typed answer out of an
    {!Answer.t}. *)
val select : 'a t -> Answer.t -> 'a option
