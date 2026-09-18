(lang dune 3.11)
(name verdict)
(version 0.1.3)
(generate_opam_files true)
(license MIT)
(authors "Jones Melton <opam@jonesmelton.com>")
(maintainers "Jones Melton <opam@jonesmelton.com>")
(source (github jonesmelton/verdict))
(package
 (name verdict)
 (synopsis "Unofficial Eio-native OCaml client for TypeSafe AI's Jev")
 (description "Typed questions and answers, bounded HTTP requests, TLS, and retry policies for the TypeSafe AI Jev model. Requires OCaml 5.2 or newer.")
 (depends
  (ocaml (>= 5.2.0))
  (eio (>= 1.5))
  (cohttp (>= 5.0.0))
  (cohttp-eio (>= 6.3.0))
  ipaddr
  domain-name
  fmt
  (tls-eio (>= 2.0.0))
  ca-certs
  (mirage-crypto-rng (>= 1.0.0))
  cstruct
  (yojson (>= 3.0.0))
  uri
  logs
  (http-date (>= 0.2))
  (eio_main :with-test)
  (alcotest :with-test)
  (qcheck-core :with-test)))
