type t =
  { api_key : string
  ; base_url : Uri.t
  ; model : string
  ; timeout : float
  ; max_response_bytes : int
  ; retry : Retry.t
  }

let nonblank = function
  | Some s when String.trim s <> "" -> Some (String.trim s)
  | _ -> None
;;

let resolve getenv explicit env fallback =
  match nonblank explicit with
  | Some key -> key
  | None -> Option.value ~default:fallback (nonblank (getenv env))
;;

let is_loopback_v4 addr = String.starts_with ~prefix:"127." (Ipaddr.V4.to_string addr)

let is_loopback host =
  match Ipaddr.of_string host with
  | Ok (V4 addr) -> is_loopback_v4 addr
  | Ok (V6 addr) ->
    (match Ipaddr.v4_of_v6 addr with
     | Some mapped -> is_loopback_v4 mapped
     | None -> Ipaddr.V6.compare addr Ipaddr.V6.localhost = 0)
  | Error _ -> String.equal (String.lowercase_ascii host) "localhost"
;;

let valid_host host = Result.is_ok (Host.of_string host)

let valid_path u =
  match Uri.path u with
  | "" -> true
  | path -> String.starts_with ~prefix:"/" path
;;

let valid_url u =
  match Uri.scheme u with
  | exception Invalid_argument _ -> false
  | None -> false
  | Some scheme ->
    (match Uri.host u with
     | None -> false
     | Some host ->
       (scheme = "https" || (scheme = "http" && is_loopback host))
       && valid_host host
       && valid_path u
       && Uri.userinfo u = None
       && Uri.verbatim_query u = None
       && Uri.fragment u = None
       &&
         (match Uri.port u with
         | None -> true
         | Some p -> p > 0 && p <= 65535))
;;

let create
      ?api_key
      ?base_url
      ?model
      ?(timeout = 10.0)
      ?(max_response_bytes = 8 * 1024 * 1024)
      ?(retry = Retry.default)
      ?(getenv = Sys.getenv_opt)
      ()
  =
  let api_key = resolve getenv api_key "TYPESAFE_API_KEY" "" in
  let base = resolve getenv base_url "TYPESAFE_BASE_URL" "https://api.typesafe.ai" in
  let model = resolve getenv model "TYPESAFE_DEFAULT_MODEL" "jev-latest" in
  let bad message = Error (Error.Configuration message) in
  if api_key = ""
  then bad "provide api_key or set TYPESAFE_API_KEY"
  else if String.exists (fun c -> Char.code c < 33 || Char.code c > 126) api_key
  then bad "API key contains invalid header characters"
  else if (not (Float.is_finite timeout)) || timeout <= 0.0
  then bad "timeout must be finite and positive"
  else if max_response_bytes <= 0 || max_response_bytes >= Sys.max_string_length
  then bad "max_response_bytes is outside the supported range"
  else if String.exists (fun c -> Char.code c <= 32 || Char.code c = 127) base
  then bad "base URL contains whitespace or control characters"
  else if String.length model = 0 || String.trim model <> model
  then bad "model must not be empty or padded with whitespace"
  else (
    match Uri.of_string base with
    | exception Invalid_argument _ -> bad "invalid base URL"
    | u when valid_url u ->
      Ok { api_key; base_url = u; model; timeout; max_response_bytes; retry }
    | _ ->
      bad
        "base URL must be HTTPS (or loopback HTTP), with a valid host, an absolute path \
         prefix or none, and no userinfo, query, or fragment")
;;

let api_key t = t.api_key
let base_url t = t.base_url
let model t = t.model
let timeout t = t.timeout
let max_response_bytes t = t.max_response_bytes
let retry t = t.retry
