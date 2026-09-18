type t =
  { transport : Transport.t
  ; config : Config.t
  }

let create ?random ~net ~clock config =
  Result.map
    (fun transport -> { transport; config })
    (Transport.make ?random ~net ~clock config)
;;

let config t = t.config
let request_id = Error.request_id

let json_of_body request_id body =
  match Yojson.Safe.from_string body with
  | `Assoc _ as json -> Ok json
  | `Null ->
    Error (Error.Decode { path = ""; message = "missing response body"; request_id })
  | _ ->
    Error (Error.Decode { path = ""; message = "expected a JSON object"; request_id })
  | exception (Yojson.Json_error _ | Invalid_argument _) ->
    Error (Error.Decode { path = ""; message = "malformed JSON"; request_id })
;;

let call t ~sw ~meth ~path body =
  Result.bind (Transport.request t.transport ~sw ~meth ~path ~body) (fun (raw, headers) ->
    let id = request_id headers in
    Result.map (fun json -> json, id) (json_of_body id raw))
;;

let resolve_model t = function
  | None -> Ok (Config.model t.config)
  | Some model ->
    if String.trim model = "" || String.trim model <> model
    then Error (Error.Configuration "model must not be blank or padded")
    else Ok model
;;

let evaluate t ~sw ?model request =
  Result.bind (resolve_model t model) (fun model ->
    let body = Yojson.Safe.to_string (Request.to_yojson request ~model) in
    Result.bind (call t ~sw ~meth:`POST ~path:"/v1/systemone" (Some body))
    @@ fun (json, id) ->
    Response.of_yojson ?request_id:id ~asked:(Request.questions request) json)
;;

let list_models t ~sw () =
  Result.bind (call t ~sw ~meth:`GET ~path:"/v1/models" None) (fun (json, id) ->
    Model.list_of_yojson json |> Result.map_error (Error.attach_request_id id))
;;
