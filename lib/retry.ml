type t =
  { max_retries : int
  ; backoff_initial : float
  ; backoff_max : float
  ; jitter : float
  ; retry_after_cap : float
  }

let default =
  { max_retries = 2
  ; backoff_initial = 0.5
  ; backoff_max = 5.0
  ; jitter = 0.25
  ; retry_after_cap = 120.0
  }
;;

let max_retries t = t.max_retries

let create
      ?(max_retries = default.max_retries)
      ?(backoff_initial = default.backoff_initial)
      ?(backoff_max = default.backoff_max)
      ?(jitter = default.jitter)
      ?(retry_after_cap = default.retry_after_cap)
      ()
  =
  if max_retries < 0
  then Error (Error.Configuration "max_retries must be a non-negative integer")
  else if (not (Float.is_finite backoff_initial)) || backoff_initial < 0.0
  then
    Error
      (Error.Configuration
         "backoff_initial must be a non-negative, finite number of seconds")
  else if (not (Float.is_finite backoff_max)) || backoff_max < 0.0
  then
    Error
      (Error.Configuration "backoff_max must be a non-negative, finite number of seconds")
  else if (not (Float.is_finite jitter)) || jitter < 0.0 || jitter > 1.0
  then Error (Error.Configuration "jitter must be between zero and one")
  else if (not (Float.is_finite retry_after_cap)) || retry_after_cap < 0.0
  then
    Error
      (Error.Configuration
         "retry_after_cap must be a non-negative, finite number of seconds")
  else Ok { max_retries; backoff_initial; backoff_max; jitter; retry_after_cap }
;;

let round_millis x = Float.floor ((x *. 1000.0) +. 0.5) /. 1000.0

let clamp_unit x =
  if Float.is_finite x then if x < 0.0 then 0.0 else if x > 1.0 then 1.0 else x else 0.0
;;

let backoff t ~attempt ~random =
  if t.backoff_initial = 0.0 || t.backoff_max = 0.0
  then 0.0
  else (
    let threshold = Float.log2 t.backoff_max -. Float.log2 t.backoff_initial in
    let exponential =
      if float_of_int attempt >= threshold
      then t.backoff_max
      else t.backoff_initial *. (2.0 ** float_of_int attempt)
    in
    let delay = exponential *. (1.0 -. (clamp_unit random *. t.jitter)) in
    Float.min exponential (round_millis delay))
;;

let is_leap_year year = (year mod 4 = 0 && year mod 100 <> 0) || year mod 400 = 0

let days_in_month year = function
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | 4 | 6 | 9 | 11 -> 30
  | 2 -> if is_leap_year year then 29 else 28
  | _ -> 0
;;

let days_from_civil year month day =
  let year = if month <= 2 then year - 1 else year in
  let era = (if year >= 0 then year else year - 399) / 400 in
  let year_of_era = year - (era * 400) in
  let month_prime = (month + 9) mod 12 in
  let day_of_year = (((153 * month_prime) + 2) / 5) + day - 1 in
  let day_of_era =
    (year_of_era * 365) + (year_of_era / 4) - (year_of_era / 100) + day_of_year
  in
  (era * 146097) + day_of_era - 719468
;;

let epoch_seconds year month day hours minutes seconds =
  float_of_int
    ((days_from_civil year month day * 86400) + (hours * 3600) + (minutes * 60) + seconds)
;;

let normalize_two_digit_year year =
  if year < 100
  then (
    let year = year + 1900 in
    if year < 1969 then year + 100 else year)
  else year
;;

let valid_dates year month day hours minutes seconds =
  month >= 1
  && month <= 12
  && day >= 1
  && day <= days_in_month year month
  && hours >= 0
  && hours <= 23
  && minutes >= 0
  && minutes <= 59
  && seconds >= 0
  && seconds <= 60
;;

let parse_http_date raw =
  match Http_date.decode raw with
  | `IMF (_, (year, month, day), (hours, minutes, seconds)) ->
    if valid_dates year month day hours minutes seconds
    then Some (epoch_seconds year month day hours minutes seconds)
    else None
  | `RFC850 (_, (year, month, day), (hours, minutes, seconds)) ->
    let year = normalize_two_digit_year year in
    if valid_dates year month day hours minutes seconds
    then Some (epoch_seconds year month day hours minutes seconds)
    else None
  | `ASCTIME (_, (year, month, day), (hours, minutes, seconds)) ->
    if valid_dates year month day hours minutes seconds
    then Some (epoch_seconds year month day hours minutes seconds)
    else None
  | exception Invalid_argument _ -> None
;;

let header_value headers name = Option.map String.trim (Cohttp.Header.get headers name)

let parse_number raw =
  let raw = if raw = "" then "0" else raw in
  float_of_string_opt raw
;;

let from_millis headers =
  match header_value headers "retry-after-ms" with
  | None -> None
  | Some raw ->
    (match parse_number raw with
     | Some value when Float.is_finite value && value >= 0.0 ->
       let seconds = value *. 0.001 in
       if Float.is_finite seconds then Some seconds else None
     | _ -> None)
;;

let from_seconds ~now headers =
  match header_value headers "retry-after" with
  | None -> None
  | Some raw ->
    (match parse_number raw with
     | Some value when Float.is_finite value && value >= 0.0 -> Some value
     | Some _ -> None
     | None ->
       (match parse_http_date raw with
        | None -> None
        | Some epoch ->
          if Float.is_finite now
          then (
            let delta = epoch -. now in
            if Float.is_finite delta then Some (Float.max 0.0 delta) else None)
          else None))
;;

let retry_after t ~now ~headers =
  match
    match from_millis headers with
    | Some seconds -> Some seconds
    | None -> from_seconds ~now headers
  with
  | None -> None
  | Some seconds -> Some (Float.min t.retry_after_cap seconds)
;;

let delay t ~attempt ~random ~now ~headers =
  match retry_after t ~now ~headers with
  | Some seconds -> seconds
  | None -> backoff t ~attempt ~random
;;
