type t =
  { http : Cohttp_eio.Client.t
  ; config : Config.t
  ; random : unit -> float
  ; now : unit -> float
  ; sleep : float -> unit
  ; timeout :
      (unit -> (string * Cohttp.Header.t, Error.t) result)
      -> (string * Cohttp.Header.t, Error.t) result
  }

let rng_lock = Mutex.create ()

let ensure_rng () =
  Mutex.lock rng_lock;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock rng_lock)
    (fun () ->
       match Mirage_crypto_rng.default_generator () with
       | _ -> ()
       | exception Mirage_crypto_rng.No_default_generator ->
         Mirage_crypto_rng_unix.use_default ())
;;

let jitter () =
  ensure_rng ();
  let seed = Mirage_crypto_rng.generate 16 in
  let state =
    Random.State.make (Array.init (String.length seed) (fun i -> Char.code seed.[i]))
  in
  fun () -> Random.State.float state 1.0
;;

let src = Logs.Src.create "verdict.transport" ~doc:"Verdict SDK transport"

module Log = (val Logs.src_log src : Logs.LOG)

let resolve_endpoint config =
  match Uri.host (Config.base_url config) with
  | None -> Error "the base URL has no host"
  | Some host -> Host.of_string host
;;

let https_connect tls target flow =
  match target with
  | `Ip ip -> Tls_eio.client_of_flow tls ~ip flow
  | `Name host -> Tls_eio.client_of_flow tls ~host flow
;;

let make ?random ~net ~clock config =
  let random = Option.value ~default:(jitter ()) random in
  let https =
    if Uri.scheme (Config.base_url config) <> Some "https"
    then Ok None
    else (
      ensure_rng ();
      match Ca_certs.authenticator () with
      | Error (`Msg reason) ->
        Error (Error.Configuration ("could not load the CA trust store: " ^ reason))
      | Ok authenticator ->
        (match Tls.Config.client ~authenticator () with
         | Error (`Msg reason) ->
           Error
             (Error.Configuration ("could not construct TLS configuration: " ^ reason))
         | Ok tls ->
           (match resolve_endpoint config with
            | Error reason ->
              Error (Error.Configuration ("could not use the base URL host: " ^ reason))
            | Ok target -> Ok (Some (fun _ flow -> https_connect tls target flow)))))
  in
  Result.map
    (fun https ->
       { http = Cohttp_eio.Client.make ~https net
       ; config
       ; random
       ; now = (fun () -> Eio.Time.now clock)
       ; sleep = Eio.Time.sleep clock
       ; timeout = (fun f -> Eio.Time.with_timeout_exn clock (Config.timeout config) f)
       })
    https
;;

let headers t attempt =
  let h =
    Cohttp.Header.of_list
      [ "authorization", "Bearer " ^ Config.api_key t.config
      ; "accept", "application/json"
      ; "accept-encoding", "identity"
      ; "content-type", "application/json"
      ; "user-agent", "verdict-ocaml/" ^ Version.sdk_version
      ; "x-typesafe-sdk", "verdict-ocaml/" ^ Version.sdk_version
      ; "x-typesafe-runtime", "OCaml/" ^ Sys.ocaml_version
      ; "connection", "close"
      ]
  in
  if attempt = 0
  then h
  else Cohttp.Header.add h "x-typesafe-retry-count" (string_of_int attempt)
;;

let bounded ?expected limit flow =
  let buffer = Buffer.create (min limit 4096) in
  let bytes = Cstruct.create (min (limit + 1) 4096) in
  let rec loop () =
    match Eio.Flow.single_read flow bytes with
    | n ->
      if n > limit - Buffer.length buffer
      then Error (Error.Response_too_large limit)
      else (
        Buffer.add_string buffer (Cstruct.to_string ~len:n bytes);
        loop ())
    | exception End_of_file ->
      let received = Buffer.length buffer in
      (match expected with
       | Some n when n > received ->
         Error
           (Error.Connection
              (Printf.sprintf "response body truncated after %d of %d bytes" received n))
       | _ -> Ok (Buffer.contents buffer))
  in
  loop ()
;;

let declared_length headers =
  match Cohttp.Header.get headers "content-length" with
  | Some raw ->
    (match int_of_string_opt (String.trim raw) with
     | Some n when n >= 0 -> Some n
     | _ -> None)
  | None -> None
;;

let join prefix path =
  let rec trim n = if n > 0 && prefix.[n - 1] = '/' then trim (n - 1) else n in
  String.sub prefix 0 (trim (String.length prefix)) ^ path
;;

let request t ~sw ~meth ~path ~body =
  let base = Config.base_url t.config in
  let uri = Uri.with_path base (join (Uri.path base) path) in
  let retry = Config.retry t.config in
  let rec sleep ~remaining =
    if remaining > 0.0
    then (
      let slice = Float.min 0.5 remaining in
      t.sleep slice;
      Eio.Switch.check sw;
      sleep ~remaining:(remaining -. slice))
  in
  let rec loop attempt =
    Eio.Switch.check sw;
    let response_headers = ref (Cohttp.Header.init ()) in
    let result =
      try
        t.timeout (fun () ->
          Eio.Switch.run (fun attempt_sw ->
            let body = Option.map Cohttp_eio.Body.of_string body in
            let response, flow =
              Cohttp_eio.Client.call
                t.http
                ~sw:attempt_sw
                ~headers:(headers t attempt)
                ?body
                meth
                uri
            in
            let headers = Cohttp.Response.headers response in
            response_headers := headers;
            let status = Cohttp.Code.code_of_status (Cohttp.Response.status response) in
            Log.debug (fun m -> m "HTTP status %d (attempt %d)" status attempt);
            match
              bounded
                ?expected:(declared_length headers)
                (Config.max_response_bytes t.config)
                flow
            with
            | Error _ as e -> e
            | Ok body ->
              if status >= 200 && status < 300
              then Ok (body, headers)
              else Error (Error.of_response ~status ~headers ~body)))
      with
      | exn -> Error (Error.of_exn ~timeout:(Config.timeout t.config) exn)
    in
    match result with
    | Error e when Error.retryable e && attempt < Retry.max_retries retry ->
      let delay =
        Retry.delay
          retry
          ~attempt
          ~random:(t.random ())
          ~now:(t.now ())
          ~headers:!response_headers
      in
      Log.debug (fun m -> m "retrying attempt %d after %.3fs" (attempt + 1) delay);
      sleep ~remaining:delay;
      loop (attempt + 1)
    | _ -> result
  in
  loop 0
;;
