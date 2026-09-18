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

let retryable_statuses = [ 408; 429; 529 ]

let retryable_status status =
  List.mem status retryable_statuses || (status >= 500 && status < 600)
;;

let member key = function
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None
;;

let nonempty = function
  | Some s when s <> "" -> Some s
  | _ -> None
;;

let string_field json =
  match json with
  | `String s when s <> "" -> Some s
  | _ -> None
;;

let object_message = function
  | `Assoc _ as obj ->
    (match member "message" obj with
     | Some msg -> string_field msg
     | None -> None)
  | _ -> None
;;

let location_path = function
  | `List items ->
    items
    |> List.filter_map (function
      | `String "body" -> None
      | `String s -> Some s
      | `Int i -> Some (string_of_int i)
      | _ -> None)
    |> String.concat "."
  | _ -> ""
;;

let detail_entry = function
  | `Assoc _ as entry ->
    (match member "msg" entry with
     | Some (`String msg) ->
       let path = location_path (Option.value (member "loc" entry) ~default:`Null) in
       Some (if path = "" then msg else path ^ ": " ^ msg)
     | _ -> None)
  | _ -> None
;;

let detail_array_message = function
  | `List entries ->
    nonempty (Some (String.concat "; " (List.filter_map detail_entry entries)))
  | _ -> None
;;

let extract_message = function
  | `String s -> nonempty (Some s)
  | `Assoc _ as json ->
    let from_error =
      match member "error" json with
      | Some (`String _ as e) -> string_field e
      | Some (`Assoc _ as e) -> object_message e
      | _ -> None
    in
    (match from_error with
     | Some _ as message -> message
     | None ->
       (match member "message" json with
        | Some msg -> string_field msg
        | None ->
          (match member "detail" json with
           | Some (`String _ as d) -> string_field d
           | Some (`Assoc _ as d) -> object_message d
           | Some (`List _ as d) -> detail_array_message d
           | _ -> None)))
  | _ -> None
;;

let message_cap = 512
let body_cap = 4096

let clip_to cap text =
  if String.length text <= cap
  then text
  else (
    let last = ref cap in
    while !last > 0 && Char.code (String.unsafe_get text (pred !last)) land 0xC0 = 0x80 do
      decr last
    done;
    String.sub text 0 !last ^ "…")
;;

let clip text = clip_to message_cap text

let on_one_line text =
  text
  |> String.split_on_char '\n'
  |> List.map String.trim
  |> List.filter (fun s -> s <> "")
  |> String.concat " "
;;

let id_cap = 128

let sanitize_id = function
  | None -> None
  | Some id ->
    let id = on_one_line id in
    if id = ""
    then None
    else
      Some
        (String.init
           (Int.min id_cap (String.length id))
           (fun i -> if Char.code (String.get id i) < 33 then ' ' else String.get id i))
;;

let request_id headers = sanitize_id (Cohttp.Header.get headers "x-typesafe-request-id")

let of_response ~status ~headers ~body =
  let message =
    match Yojson.Safe.from_string body with
    | json ->
      (match extract_message json with
       | Some msg -> clip msg
       | None -> "")
    | exception Yojson.Json_error _ -> ""
    | exception Invalid_argument _ -> ""
  in
  Api
    { status
    ; message = on_one_line (clip message)
    ; request_id = request_id headers
    ; body = clip_to body_cap body
    }
;;

let retryable = function
  | Timeout _ | Connection _ -> true
  | Api { status; _ } -> retryable_status status
  | Configuration _ | Tls _ | Response_too_large _ | Decode _ -> false
;;

let rec of_exn ~timeout exn =
  match exn with
  | Eio.Time.Timeout -> Timeout timeout
  | End_of_file -> Connection "connection closed by peer"
  | Eio.Io _ -> Connection (on_one_line (Printexc.to_string exn))
  | (Tls_eio.Tls_failure _ | Tls_eio.Tls_alert _) as exn ->
    Tls (on_one_line (Printexc.to_string exn))
  (* These prefixes are untyped messages owned by cohttp-eio and cohttp, not a
     declared interface, so the mapping is re-checked on upgrade instead of
     pinned with a version bound: an unrecognised Failure is re-raised. *)
  | Failure msg
    when String.starts_with ~prefix:"connection closed by peer" msg
         || String.starts_with ~prefix:"failed to resolve" msg -> Connection msg
  | Failure msg when String.starts_with ~prefix:"Malformed response" msg ->
    Decode { path = ""; message = "malformed HTTP response: " ^ msg; request_id = None }
  | Failure msg
    when String.starts_with ~prefix:"no host specified" msg
         || String.starts_with ~prefix:"Unknown scheme" msg
         || String.starts_with ~prefix:"HTTPS not enabled" msg -> Configuration msg
  (* Eio raises this during cancellation unwinding, where the first inner
     exception is the informative one. *)
  | Eio.Exn.Multiple ((inner, _) :: _) -> of_exn ~timeout inner
  | Eio.Exn.Multiple _ -> Connection "multiple I/O failures"
  | exn -> raise exn
;;

let attach_request_id id error =
  match id, error with
  | Some id, Decode { path; message; request_id = None } ->
    Decode { path; message; request_id = Some id }
  | _ -> error
;;

let message = function
  | Configuration reason -> reason
  | Timeout seconds -> Printf.sprintf "Request timed out (timeout=%g seconds)" seconds
  | Connection reason -> if reason = "" then "Connection failed" else reason
  | Tls reason -> if reason = "" then "TLS failure" else reason
  | Response_too_large limit ->
    Printf.sprintf "Response body exceeds the maximum size of %d bytes" limit
  | Decode { path; message; request_id } ->
    let base = Printf.sprintf "Invalid response data at %s: %s" path message in
    (match request_id with
     | None -> base
     | Some id -> Printf.sprintf "%s (request_id=%s)" base id)
  | Api { status; message; request_id; _ } ->
    let base =
      if message = ""
      then Printf.sprintf "HTTP %d" status
      else Printf.sprintf "HTTP %d: %s" status message
    in
    (match request_id with
     | None -> base
     | Some id -> Printf.sprintf "%s (request_id=%s)" base id)
;;
