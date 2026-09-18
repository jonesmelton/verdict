(** A host that is usable as a TLS endpoint, either a literal IP address or a
    TLS hostname.

    {!of_string} accepts an IP address, or a name satisfying [Domain_name.host]:
    at least one label, no underscores, no leading or trailing hyphen within a
    label, and no over-long label. It requires [Domain_name.host] rather than
    merely [Domain_name.of_string], because the latter accepts raw domain names
    that [Domain_name.host_exn] later rejects with [Invalid_argument]. At least
    one label is required because [Domain_name.host] accepts the empty name.

    Both {!Config} and {!Transport} validate through this function, so
    configuration and transport cannot disagree about what a usable host is. *)

type t =
  [ `Ip of Ipaddr.t
  | `Name of [ `host ] Domain_name.t
  ]

(** [of_string host] is [Ok] for an IP address or a valid TLS hostname, and
    [Error reason] otherwise. *)
val of_string : string -> (t, string) result
