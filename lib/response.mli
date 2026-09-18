(** A decoded [POST /v1/systemone] response. *)

type t

(** [of_yojson ?request_id ?asked json] decodes [json].

    An answer whose [type] tag is not a known kind is skipped with a warning on
    the [verdict.codec] log source instead of failing the response, so an
    addition on the server does not break an older client; the JSON for such an
    answer stays reachable through {!raw_answer}. The response must contain at
    least one answer.

    When [asked] is supplied, each question is checked against the response. A
    missing answer, an answer of the wrong kind, or a [type] that cannot serve
    the question becomes [Error.Decode] with [path = "answers.<name>"], which
    keeps "did I get the answer I asked for" out of the caller's control flow.
    {!Client.evaluate} always passes the questions it sent. [request_id] is
    attached to every decode error this function produces. *)
val of_yojson
  :  ?request_id:string
  -> ?asked:Question.packed list
  -> Yojson.Safe.t
  -> (t, Error.t) result

(** [find t q] recovers the typed answer for [q]. It is [None] when the server
    answered only a subset of the requested names, and also when the answer's
    kind does not match [q]. *)
val find : t -> 'a Question.t -> 'a option

(** The response exactly as received. *)
val raw : t -> Yojson.Safe.t

(** The received JSON for one answer name, including an answer that
    {!of_yojson} skipped because its [type] is unknown. *)
val raw_answer : t -> string -> Yojson.Safe.t option

val model : t -> string
val usage : t -> Usage.t

(** The server's [x-typesafe-request-id], sanitised as {!Error.request_id}. *)
val request_id : t -> string option
