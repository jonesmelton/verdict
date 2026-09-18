type t =
  { name : string
  ; description : string
  ; release_date : string
  }

let list_of_yojson json =
  Decode.protect (fun () ->
    let open Decode in
    get list "" "models" json
    |> List.mapi (fun i j ->
      let path = "models." ^ string_of_int i in
      { name = get string path "name" j
      ; description = get string path "description" j
      ; release_date = get string path "release_date" j
      }))
;;
