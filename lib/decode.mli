(** Yojson decoding combinators.

    Every function here either returns a decoded value or raises {!Invalid},
    which carries an [Error.Decode] naming the JSON path it failed at. Paths
    are dotted and extended by {!at} as the decoders descend, so a failure
    reports where the input went wrong ([answers.spam.noul]) rather than what
    the expected OCaml type was. Writing each codec against these combinators
    keeps those paths accurate without threading them by hand.

    Nothing here logs, truncates, or attaches a request id: a decode error
    reaches the caller intact, and {!Error.attach_request_id} is what fills in
    the request id once one is known. *)

(** Raised on malformed input. The payload is always an [Error.Decode] with
    [request_id = None]. *)
exception Invalid of Error.t

(** [fail path message] raises {!Invalid} at [path]. Raising anything else
    defeats {!protect}, which converts this exception and no other. *)
val fail : string -> string -> 'a

(** [at path key] extends [path] by one field, so nested failures report a
    dotted path. [at "" key] is [key]. *)
val at : string -> string -> string

(** [object_ path json] is [json]'s fields, requiring an object.

    Duplicate keys are rejected rather than resolved last-wins. The wire format
    does not use them, and silently keeping one would make the decoded value
    depend on key order. *)
val object_ : string -> Yojson.Safe.t -> (string * Yojson.Safe.t) list

(** [field path key json] is the value at [key], requiring [json] to be an
    object containing it. The object is checked with {!object_}, so duplicate
    keys are rejected here as well. *)
val field : string -> string -> Yojson.Safe.t -> Yojson.Safe.t

(** A JSON string. A number, boolean, or null is not coerced. *)
val string : string -> Yojson.Safe.t -> string

(** A JSON number as a float: an integer literal is widened. Anything
    non-finite is rejected, as is an integer literal too large for [int], which
    arrives as Yojson's [`Intlit] and would otherwise be silently rounded. *)
val number : string -> Yojson.Safe.t -> float

(** A {!Probability.t}. The range check lives here so that an out-of-range wire
    value becomes a path-carrying decode error instead of a clamped answer; the
    message comes from {!Probability.of_float}. *)
val probability : string -> Yojson.Safe.t -> Probability.t

(** A {!Confidence.t}, checked and reported as {!probability} is. *)
val confidence : string -> Yojson.Safe.t -> Confidence.t

(** A non-negative integer literal. A negative integer, a float, and an
    over-large literal are all rejected; nothing is rounded or truncated. *)
val integer : string -> Yojson.Safe.t -> int

(** The elements of a JSON array. *)
val list : string -> Yojson.Safe.t -> Yojson.Safe.t list

(** [get decode path key json] applies [decode] to the field [key] of [json],
    with the path extended by [key]. Composition through [get] is what keeps a
    nested failure's path correct. *)
val get : (string -> Yojson.Safe.t -> 'a) -> string -> string -> Yojson.Safe.t -> 'a

(** [protect f] is [Ok] the result of [f], or [Error] if it raises {!Invalid}.
    Any other exception propagates: this converts decoding failures, not
    programmer errors. *)
val protect : (unit -> 'a) -> ('a, Error.t) result
