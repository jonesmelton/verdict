(** Bounded HTTP/TLS transport.

    Internal to the SDK, but exposed so that tests can drive it directly. *)

type t

(** [make ~net ~clock config] resolves the base URL's TLS endpoint once (an
    [Ipaddr.t] or a TLS hostname) instead of per request, so an unsatisfiable
    host is [Error.Configuration] here rather than a raise inside the
    per-request [~https] callback, which cannot report a [result]. The host
    cannot vary per request because every URI is built from the base URL. Also
    loads the CA trust store, and installs a default [Mirage_crypto_rng]
    generator on first use: that generator is process-global mutable state, and
    this is guarded by a mutex so two clients cannot race to seed it.

    [clock] {b must be a wall clock}: HTTP-date [Retry-After] values are
    compared against it. [random] yields jitter in \[0,1\] and defaults to a
    [Mirage_crypto_rng]-seeded state. *)
val make
  :  ?random:(unit -> float)
  -> net:_ Eio.Net.t
  -> clock:_ Eio.Time.clock
  -> Config.t
  -> (t, Error.t) result

(** [request t ~sw ~meth ~path ~body] sends one request, retrying per
    {!Config.retry} and {!Error.retryable} until the attempts are exhausted or
    an error is not retryable.

    [path] is appended to the base URL's path. Requests send
    [accept-encoding: identity] because nothing here decompresses a body, so a
    [Content-Encoding] applied by an intermediary would otherwise surface as a
    decode failure instead of a transport failure. Retried attempts carry
    [x-typesafe-retry-count]; the first attempt does not, so the server can
    correlate them.

    A body that ends before its declared [content-length] is a retryable
    [Error.Connection], not a successful short read; one larger than
    {!Config.max_response_bytes} is [Error.Response_too_large]. [sw] is checked
    before the first attempt and between backoff slices, so cancellation is
    honoured promptly. *)
val request
  :  t
  -> sw:Eio.Switch.t
  -> meth:Cohttp.Code.meth
  -> path:string
  -> body:string option
  -> (string * Cohttp.Header.t, Error.t) result
