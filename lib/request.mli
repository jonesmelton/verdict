type t

val create
  :  ?model:string
  -> state:Content.t
  -> Question.packed list
  -> (t, Error.t) result

val model : t -> string option
val questions : t -> Question.packed list
val to_yojson : t -> model:string -> Yojson.Safe.t
