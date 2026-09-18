type t =
  { name : string
  ; description : string
  ; release_date : string
  }

val list_of_yojson : Yojson.Safe.t -> (t list, Error.t) result
