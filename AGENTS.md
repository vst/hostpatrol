# Repository Guidelines

## Project Structure & Module Organization

- `src/HostPatrol/...` hosts the library; add modules under the existing
  namespace and keep general-purpose helpers in `Zamazingo.*`, such as SSH.
- `app/Main.hs` wires the CLI entrypoint; `scripts/` stores POSIX sh snippets
  shipped to hosts, review `scripts/README.md` before changes.
- Tests live in `test/doctest` and `test/spec`; mirror source structure when
  adding specs. `nix/` handles tooling, `website/` contains the Next.js-based
  Website source-code.

## Build, Test, and Development Commands

- `nix develop` provides cabal, formatters, and analyzers; pair with
  `direnv allow` for automatic activation.
- `cabal dev-test-build` is the required pre-push check: it runs formatters,
  `hlint`, `stan`, `weeder`, builds, smoke-tests the CLI, runs `cabal v1-test`,
  and generates Haddocks.
- Use `cabal dev-test-build -c` for a clean rebuild, `nix build .#hostpatrol`
  for release binaries, and `./build-static.sh --platform linux/amd64` when you
  need a Docker-backed static executable.

## Coding Style & Naming Conventions

- Fourmolu enforces two-space indentation, leading commas, and ASCII syntax; run
  it instead of hand-formatting.
- Modules follow `HostPatrol.<Area>` or `Zamazingo.<Utility>`; types use
  `PascalCase`, functions `camelCase`, CLI flags kebab-case.
- Shell scripts stay POSIX `sh`, lower-case-kebab named, idempotent, and free of
  stateful mutations.

## Testing Guidelines

- Keep doctest examples beside the functions they exercise and confirm with
  `cabal test hostpatrol-doctest`.
- Grow `test/spec/Spec.hs` using Hspec-style `describe/it` groups that mirror
  the module under test.
- For host scripts, validate with
  `cat scripts/<name>.sh | ssh -T <host> 'sh -s --'` against a safe target and
  capture sample output.

## Commit & Pull Request Guidelines

- Follow the conventional commit style from history (`refactor(build): …`);
  scopes should reference directories or modules.
- Every commit must pass `cabal dev-test-build` and include generated or
  formatted artifacts.
- Pull requests need a concise summary, validation notes, linked issues (e.g.
  `Closes #42`), and UI screenshots when touching `website/`.

## Remote Script Safety

- Preserve portability, minimal dependencies, and the read-only contract
  documented in `scripts/README.md`.
- Guard advanced tooling behind capability checks and document anything
  non-obvious with brief inline comments.
