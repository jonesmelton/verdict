(** A closed error taxonomy.

    Every failure is one of these cases, which is what makes {!retryable}
    decidable: a caller never has to guess whether an unfamiliar error deserves
    a retry, and a new case cannot be added without updating that function.

    {!Api} and {!constructor:Decode} carry the server's [x-typesafe-request-id] when it
    supplies one, so a support ticket can reference the exact request. Text
    taken from the server is flattened onto one line and clipped before it can
    reach a log — [Api.message] to 512 bytes, [Api.body] to 4096 bytes, and the
    request id to 128 bytes — because unbounded server text is a log-injection
    and denial-of-service vector once it is interpolated into a log line.
    Truncation happens on a UTF-8 boundary and is marked with an ellipsis. Only
    [Api.body] keeps its exact bytes, and {!message} never renders it. The API
    key is never part of any error value. *)

type t =
  | Configuration of string
  | Timeout of float
  | Connection of string
  | Response_too_large of int
  | Tls of string
  | Decode of
      { path : string
      ; message : string
      ; request_id : string option
      }
  | Api of
      { status : int
      ; message : string
      ; request_id : string option
      ; body : string
      }

(** A single-line description that never includes the API key. {!constructor:Decode}
    renders the dotted [path], and both {!constructor:Decode} and {!Api} append the
    request id when
    one is present. [Api.message] and [Api.body] are already clipped when they
    are stored, so this function does not truncate further. *)
val message : t -> string

(** The sanitised [x-typesafe-request-id] header: control characters replaced by
    spaces, flattened onto one line, clipped to 128 bytes. [None] when the
    header is absent or empty after trimming. *)
val request_id : Cohttp.Header.t -> string option

(** Classify a non-2xx response as {!Api}, extracting a message from the several
    error-body shapes the API sends. "message" is [""] when nothing usable is
    found. *)
val of_response : status:int -> headers:Cohttp.Header.t -> body:string -> t

(** Classify an exception raised by the transport, using [timeout] for the
    {!Timeout} payload.

    Only recognised I/O, TLS, EOF and cohttp-eio failures are classified;
    anything else — including an unrecognised [Failure] — is re-raised, so a
    library or programmer error cannot be laundered into a server error. Callers
    that want to observe Eio cancellation or their own exceptions see them
    unchanged. [Eio.Exn.Multiple] is classified from its first inner exception,
    because Eio raises it during cancellation unwinding. *)
val of_exn : timeout:float -> exn -> t

(** [Timeout] and [Connection] are retryable, as is {!Api} for status 408, 429,
    and anything in \[500,600\]. [Configuration], [Tls], [Response_too_large] and
    [Decode] never are: a TLS failure is a trust or configuration problem rather
    than transient noise, and retrying it would hide the cause while consuming
    the attempt budget.

    The [Api] set is wider than the one the product brief enumerates, matching
    the reference SDK: 408 is a server-side timeout and therefore transient, and
    every other 5xx is at least as temporary as the enumerated ones. It is
    deliberately not a whitelist that needs updating as the API grows. *)
val retryable : t -> bool

(** Fill in [request_id] on a {!constructor:Decode} error that has none, so that a
    malformed
    body can still be traced to a request. Other errors are returned
    unchanged. *)
val attach_request_id : string option -> t -> t
