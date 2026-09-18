type t

val text : string -> t
val of_yojson : Yojson.Safe.t -> (t, Error.t) result
val to_yojson : t -> Yojson.Safe.t
val is_blank : t -> bool
