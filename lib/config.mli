(** Validated configuration: API key, base URL, model, limits and retry policy.

    A configuration is validated on construction and never changes afterwards,
    so no later call has to re-check it. *)

type t

(** [create ()] resolves the key, base URL, and model from the explicit argument
    first, then the environment, then a default:

    - API key: [TYPESAFE_API_KEY]; there is no default, so [Error.Configuration]
      is returned when neither source supplies one.
    - base URL: [TYPESAFE_BASE_URL], default ["https://api.typesafe.ai"].
    - model: [TYPESAFE_DEFAULT_MODEL], default ["jev-latest"].

    Environment and explicit values are trimmed, and a blank value counts as
    absent. [timeout] defaults to 10.0 seconds, [max_response_bytes] to 8 MiB,
    and [retry] to {!Retry.default}.

    A base URL must be HTTPS, or HTTP on a loopback host: [localhost],
    127.0.0.0/8, [::1], and the IPv4-mapped rendering of 127.0.0.0/8. IPv6
    unique-local addresses (fc00::/7) are deliberately {e not} loopback, because
    they route within a site, so permitting cleartext HTTP to one would put the
    API key on a real network. The URL must also have a usable host, an empty or
    [/]-prefixed path, no userinfo, query, or fragment, and a port within
    \[1,65535\]; the path check exists because [Uri] silently re-reads
    ["https://1.2.3.4.5"] as host ["1.2.3.4"] with path [".5"].

    The API key must be non-empty and free of control and non-printable
    characters, because it becomes a header value. [timeout] must be finite and
    positive, [max_response_bytes] must be positive and below
    [Sys.max_string_length], and the model must be non-empty and unpadded.

    [getenv] defaults to [Sys.getenv_opt] and is injectable so that tests never
    depend on ambient process state. *)
val create
  :  ?api_key:string
  -> ?base_url:string
  -> ?model:string
  -> ?timeout:float
  -> ?max_response_bytes:int
  -> ?retry:Retry.t
  -> ?getenv:(string -> string option)
  -> unit
  -> (t, Error.t) result

(** Sent as [authorization: Bearer ...], and deliberately absent from every
    error message. *)
val api_key : t -> string

val base_url : t -> Uri.t

(** The default model. {!Client.evaluate} can override it per request. *)
val model : t -> string

(** Per-attempt timeout in seconds: connection, TLS handshake, request write,
    and body read. It does not bound the backoff sleep between attempts. *)
val timeout : t -> float

(** Cap on a response body. A body that exceeds it is reported as
    [Error.Response_too_large] rather than read partially. *)
val max_response_bytes : t -> int

val retry : t -> Retry.t
