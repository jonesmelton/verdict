(** A number in \[0,1\] expressing confidence in a whole answer.

    Not interchangeable with {!Probability.t}. A probability is one member of a
    distribution over alternatives; a confidence is a statement about the answer
    as a whole, and is carried by {!Answer.Choice.t} and {!Answer.Score.t} but
    not by {!Answer.Noul.t}.

    Values are obtained only through {!of_float}, which rejects NaN, infinities,
    and anything outside \[0,1\] with a message. When a server value fails that
    check while decoding, the rejection surfaces as a path-carrying
    [Error.Decode] naming the offending field. *)

type t

(** [of_float x] is [Ok x] when [x] is finite and within \[0,1\], and
    [Error reason] otherwise. *)
val of_float : float -> (t, string) result

(** The underlying number, always within \[0,1\]. *)
val to_float : t -> float
