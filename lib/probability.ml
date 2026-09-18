type t = float

let of_float x =
  if Float.is_finite x && x >= 0.0 && x <= 1.0
  then Ok x
  else Error "expected a probability between zero and one"
;;

let to_float t = t
