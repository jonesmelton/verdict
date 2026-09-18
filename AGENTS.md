# AGENTS.md

Instructions for agents working in this repository, split by who they act for.

## Adopting `verdict` into an OCaml codebase

You are adding this library to someone else's project. Integrate it; don't edit
it. Pin the checkout (`opam pin add verdict .`) rather than vendoring a patched
copy, and let the generated `verdict.opam` supply the dependency set.

- **Environment.** OCaml 5.2+ and Eio. The library is Eio-native: every call
  takes `~sw` and the client needs an `Eio.Net.t` and an `Eio.Time.clock`. It
  must run inside an Eio main loop (`Eio_main.run`), not from a bare `main`.
- **Clock.** `Client.create ~clock` requires a wall clock
  (`Eio.Stdenv.clock`). A monotonic clock silently mis-resolves HTTP-date
  `Retry-After` values.
- **Client lifetime.** A client is immutable and safe to share across fibers
  and domains. Create one at startup and reuse it; `create` loads the CA trust
  store and resolves the base URL's TLS endpoint, which should not happen per
  request.
- **Credentials.** The API key comes from `TYPESAFE_API_KEY` or `~api_key`, and
  is never part of an error value. Don't read `Config.api_key` for logging, and
  don't echo environment contents into diagnostics.
- **Results, not exceptions.** Every fallible entry point returns
  `(_, Error.t) result`. Don't reach for `Result.get_ok` or `Option.get` on
  these paths; propagate or match. `Error.t` is a closed variant, so an
  exhaustive match is stable across versions, and `Error.retryable` is the
  supported way to classify a failure.
- **Don't add a retry loop.** `Client.evaluate` and `Client.list_models` already
  retry per `Retry.t`, with each attempt bounded by `Config.timeout`. A wrapper
  multiplies the attempt budget and defeats request-id correlation.
- **Keep the question handles.** Answers are recovered with
  `Response.find response handle`, which returns `None` for a missing answer or
  one whose kind does not match the handle. Never key answers by string id, and
  call `Question.pack` only at the `Request.create` boundary.
- **Per-request model.** `Client.evaluate ?model` overrides the configured
  default for one request.
- **Cancellation.** Requests run under the caller's switch; abandoning one
  releases it within about half a second. Give long-lived clients a switch with
  the application's lifetime rather than one per request.
- **Logging.** Unknown answer kinds are warned on the `Logs` source
  `verdict.codec` and transport problems on `verdict.transport`. Route `Logs`
  to whatever reporter the host application already uses.
- **Internals.** `Transport`, `Host` and `Version` are exposed but internal, and
  `Decode` is a private module. Don't depend on them. Answer records are private
  and meant to be read, not constructed.

## Contributing to `verdict`

Local conventions, in addition to whatever the operator's global agent
instructions say.

- **Layout.** One module per subject, and a type with more than one associated
  function lives in a submodule with `type t` (`Answer.Choice.t`,
  `Answer.Score.t`). Every module has an `.mli`; types whose invariants matter
  are abstract or private, with a validating constructor as the only way in.
- **Dependencies.** The OCaml standard library, or a Jane Street library that
  does not depend on `base`/`core` (ppx and `@@deriving` are welcome). No
  `base`, no `core`. Adding a dependency means editing the `(depends ...)` field
  of `dune-project`.
- **Comments.** Do not add comments to source. The `.mli` odoc is the
  documentation surface and the approved home for explanation; design rationale
  belongs in the commit message, PR or ticket. Flag any file that has crept past
  a couple of comments.
- **Generated files.** `verdict.opam` is generated from `dune-project`, and
  `lib/version.ml` from the `(version ...)` field. Edit the generator, never the
  generated file.
- **Build and tests.** `opam exec -- dune build @all`, `opam exec -- dune
  runtest`, and `opam exec -- dune build @fmt` for `ocamlformat` (profile
  `janestreet`). CI also runs odoc lint and `opam lint`.
- **Test-first.** A behavioural change starts with a failing test. The suite is
  offline: loopback servers over real sockets, `Eio_mock` for deterministic
  timing, QCheck for JSON round-tripping. Live smoke tests only build under
  `VERDICT_LIVE_API=true` and spend API credits.
