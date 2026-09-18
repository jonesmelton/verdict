(** Retry policy: how many attempts, how long to wait, and how to read the
    server's [Retry-After].

    All durations are in seconds. A policy is abstract because {!create} is its
    only constructor, so its range checks cannot be bypassed by building the
    record directly. *)

type t

(** [create ()] uses the {!default} value for every omitted field.

    [max_retries] is the number of retries after the first attempt, so
    [max_retries = 2] allows three requests in total. [backoff_initial] and
    [backoff_max] bound the exponential backoff, [jitter] is the fraction of the
    delay that may be subtracted, and [retry_after_cap] caps any delay the
    server asks for.

    [Error.Configuration] unless [max_retries] is non-negative, [backoff_initial],
    [backoff_max] and [retry_after_cap] are finite and non-negative, and
    [jitter] is within \[0,1\]. *)
val create
  :  ?max_retries:int
  -> ?backoff_initial:float
  -> ?backoff_max:float
  -> ?jitter:float
  -> ?retry_after_cap:float
  -> unit
  -> (t, Error.t) result

(** [max_retries = 2], [backoff_initial = 0.5], [backoff_max = 5.0],
    [jitter = 0.25], [retry_after_cap = 120.0]. *)
val default : t

val max_retries : t -> int

(** [delay t ~attempt ~random ~now ~headers] is the number of seconds to wait
    before [attempt] (zero-based), in seconds. Exposed so retry timing can be
    tested without waiting.

    [retry-after-ms] takes precedence over [retry-after], which may be
    delta-seconds or an HTTP-date; either is capped at [retry_after_cap]. The
    reference SDK treats an empty or whitespace-only [retry-after] as a present
    value of zero and ignores a negative delta-seconds value, so this function
    does too, which is more permissive than RFC 9110. [now] is wall-clock
    seconds since the epoch, used only to resolve HTTP-date values. [random] is
    expected in \[0,1\]; jitter is subtractive, so the result never exceeds the
    un-jittered backoff. *)
val delay
  :  t
  -> attempt:int
  -> random:float
  -> now:float
  -> headers:Cohttp.Header.t
  -> float
