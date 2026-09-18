exception Invalid of Error.t

let fail path message =
  raise (Invalid (Error.Decode { path; message; request_id = None }))
;;

let at path key = if path = "" then key else path ^ "." ^ key

let object_ path = function
  | `Assoc xs ->
    let names = List.map fst xs in
    if List.length names <> List.length (List.sort_uniq String.compare names)
    then fail path "duplicate object key";
    xs
  | _ -> fail path "expected object"
;;

let field path key json =
  match List.assoc_opt key (object_ path json) with
  | Some x -> x
  | None -> fail (at path key) "missing field"
;;

let string path = function
  | `String s -> s
  | _ -> fail path "expected string"
;;

let number path = function
  | `Int i -> float_of_int i
  | `Float f when Float.is_finite f -> f
  | _ -> fail path "expected finite number"
;;

let probability path j =
  let n = number path j in
  match Probability.of_float n with
  | Ok p -> p
  | Error message -> fail path message
;;

let confidence path j =
  let n = number path j in
  match Confidence.of_float n with
  | Ok c -> c
  | Error message -> fail path message
;;

let integer path = function
  | `Int i when i >= 0 -> i
  | _ -> fail path "expected nonnegative integer"
;;

let list path = function
  | `List xs -> xs
  | _ -> fail path "expected array"
;;

let get decode path key j = decode (at path key) (field path key j)

let protect f =
  try Ok (f ()) with
  | Invalid e -> Error e
;;
