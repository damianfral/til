# Nix flake for a Haskell CLI application

This flake contains a Nix package and a development shell for a sample Haskell
CLI application called `hello`.

The Haskell package is wrapped with `generateOptparseApplicativeCompletions`,
which adds shell completion scripts. These scripts will be automatically picked
up if the resulting derivation is installed.

The development shell provides the following tools for Haskell development:

- [cabal-install](https://www.haskell.org/cabal/)
- [haskell-language-server](https://github.com/haskell/haskell-language-server)
- [ghcid](https://github.com/ndmitchell/ghcid)
- [pre-commit-hooks.nix](https://github.com/cachix/pre-commit-hooks.nix)
  - [actionlint](https://github.com/rhysd/actionlint)
  - [hlint](https://github.com/ndmitchell/hlint)
  - [hpack](https://github.com/sol/hpack)
  - [nil](https://github.com/oxalica/nil)
  - [nixpkgs-fmt](https://github.com/nix-community/nixpkgs-fmt)
  - [ormolu](https://github.com/tweag/ormolu)
  - [statix](https://github.com/nerdypeppercom/jez/statix)
