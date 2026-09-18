(** A number in \[0,1\] that is one value of a probability distribution.

    The representation is shared with {!Confidence.t}, but the two are not
    interchangeable: a probability is one member of a distribution, while a
    confidence is a claim about an answer as a whole. They never belong in the
    same expression, so they are separate abstract types.

    Values are obtained only through {!of_float}, which rejects NaN, infinities,
    and anything outside \[0,1\] with a message. When a server value fails that
    check while decoding, the rejection surfaces as a path-carrying
    [Error.Decode] naming the offending field, so an out-of-range value is
    never silently clamped. *)

type t

(** [of_float x] is [Ok x] when [x] is finite and within \[0,1\], and
    [Error reason] otherwise. *)
val of_float : float -> (t, string) result

(** The underlying number, always within \[0,1\]. *)
val to_float : t -> float
