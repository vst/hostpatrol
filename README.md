# Lazy Hacker's Linux Host Patrol

> **TODO** Provide minimum viable documentation.

## Development

The codebase comes with a Nix shell. You can use `direnv` for conveience:

```sh
direnv allow
```

Big, long format, lint, build and test command for the impatient:

```sh
hpack &&
    direnv reload &&
    fourmolu -i app/ src/ test/ &&
    prettier --write . &&
    find . -iname "*.nix" -not -path "*/nix/sources.nix" -print0 | xargs --null nixpkgs-fmt &&
    hlint app/ src/ test/ &&
    cabal build -O0 &&
    cabal run -O0 lhp -- --version &&
    cabal v1-test &&
    cabal haddock -O0
```

For testing and building:

```sh
dev-test-build
```

<!-- REFERENCES -->
