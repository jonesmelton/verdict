let ( let* ) = Result.bind

let () =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let open Verdict in
      let result =
        let* config = Config.create () in
        let* client =
          Client.create ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) config
        in
        let* models = Client.list_models client ~sw () in
        List.iter
          (fun (model : Model.t) ->
             Printf.printf "%s\t%s\t%s\n" model.name model.release_date model.description)
          models;
        Ok ()
      in
      match result with
      | Ok () -> ()
      | Error e ->
        prerr_endline (Error.message e);
        exit 1))
;;
