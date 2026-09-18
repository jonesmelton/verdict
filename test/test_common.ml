open Verdict

let contains haystack needle =
  let n = String.length needle in
  let h = String.length haystack in
  let rec go i = i + n <= h && (String.sub haystack i n = needle || go (i + 1)) in
  n = 0 || go 0
;;

let ok = function
  | Ok x -> x
  | Error e -> Alcotest.failf "unexpected error: %s" (Error.message e)
;;

let describe show = function
  | Ok x -> show x
  | Error e -> Error.message e
;;
