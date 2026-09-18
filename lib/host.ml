type t =
  [ `Ip of Ipaddr.t
  | `Name of [ `host ] Domain_name.t
  ]

let of_string host =
  match Ipaddr.of_string host with
  | Ok ip -> Ok (`Ip ip)
  | Error _ ->
    (match Domain_name.of_string host with
     | Error (`Msg reason) -> Error reason
     | Ok name ->
       (match Domain_name.host name with
        | Error (`Msg reason) -> Error reason
        | Ok name ->
          if Domain_name.to_strings name = [] then Error "empty host" else Ok (`Name name)))
;;
