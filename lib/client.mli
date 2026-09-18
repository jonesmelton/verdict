(** Requests against the TypeSafe AI API, with the retry policy applied
    internally.

    A client is immutable once created and may be shared across fibers and
    domains. *)

type t

(** [create ~net ~clock config] builds a client. It loads the CA trust store and
    resolves the base URL's TLS endpoint once, here, so an unsatisfiable host
    becomes [Error.Configuration] rather than an exception on the request path.

    [clock] {b must be a wall clock}, not a monotonic one: HTTP-date
    [Retry-After] values are compared against it, and a monotonic clock would
    silently mis-resolve them. [clock] also supplies the backoff sleep.

    [random] yields jitter in \[0,1\]. The default is a state seeded from
    [Mirage_crypto_rng] rather than the stdlib's default state, whose
    per-domain initialisation is deterministic and would make every process
    retry in lockstep. The parameter exists so retry timing can be tested
    without waiting on entropy. *)
val create
  :  ?random:(unit -> float)
  -> net:_ Eio.Net.t
  -> clock:_ Eio.Time.clock
  -> Config.t
  -> (t, Error.t) result

val config : t -> Config.t

(** [evaluate t ~sw ?model request] posts [request] and decodes the response.

    Everything runs under [sw]. Each attempt opens its own connection inside a
    child switch and sends [connection: close], so a retry cannot leave a socket
    or TLS record layer attached to [sw]. Each attempt is bounded by
    {!Config.timeout}; the backoff sleep between attempts is not, so a server
    asking for a longer [Retry-After] than the attempt timeout is waited out
    rather than cut short. The sleep is sliced and [sw] is checked between
    slices, so a cancelled caller is released within half a second.

    [model] overrides {!Config.model} for this request only;
    [Error.Configuration] is returned if it is blank or padded. Retries are
    applied per {!Retry.t} and {!Error.retryable}.

    The response is validated against [request]'s questions, so a missing
    answer, an unparseable answer, or an answer whose [type] cannot serve the
    question is [Error.Decode] with [path = "answers.<name>"], carrying the
    server request id when it was supplied. *)
val evaluate
  :  t
  -> sw:Eio.Switch.t
  -> ?model:string
  -> Request.t
  -> (Response.t, Error.t) result

(** [list_models t ~sw ()] performs [GET /v1/models] under the same retry
    policy, and belongs to the same [sw] lifetime as {!evaluate}. *)
val list_models : t -> sw:Eio.Switch.t -> unit -> (Model.t list, Error.t) result
