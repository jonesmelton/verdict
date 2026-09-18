type t = Yojson.Safe.t

let text s = `String s

let rec valid = function
  | `Null | `Bool _ | `Int _ | `String _ -> true
  | `Float f -> Float.is_finite f
  | `List xs -> List.for_all valid xs
  | `Assoc xs ->
    let keys = List.map fst xs in
    List.length keys = List.length (List.sort_uniq String.compare keys)
    && List.for_all (fun (_, v) -> valid v) xs
  | `Intlit s ->
    (try
       ignore (Yojson.Safe.from_string s);
       String.length s > 0
       && String.for_all
            (function
              | '0' .. '9' | '-' -> true
              | _ -> false)
            s
     with
     | Yojson.Json_error _ -> false)
;;

let of_yojson j =
  match j with
  | (`String _ | `Assoc _ | `List _) when valid j -> Ok j
  | _ ->
    Error
      (Error.Configuration
         "content must be a string, object, or array containing valid JSON")
;;

let to_yojson t = t

let is_blank = function
  | `String s -> String.trim s = ""
  | `Assoc [] | `List [] -> true
  | _ -> false
;;
