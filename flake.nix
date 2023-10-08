{
  description = "TBD";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    feedback.url = "github:NorfairKing/feedback";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    flake-utils,
    nix-filter,
    pre-commit-hooks,
    ...
  }: let
    pkgsFor = system:
      import nixpkgs {
        inherit system;
        overlays = [self.overlays.default];
      };
    filteredSrc = nix-filter.lib {
      root = ./.;
      include = [
        "src/"
        "test/"
        "package.yaml"
        "LICENSE"
      ];
    };
  in
    {
      overlays.default = final: prev:
        with final.haskell.lib; {
          haskellPackages = prev.haskellPackages.override (old: {
            overrides =
              final.lib.composeExtensions
              (old.overrides or (_: _: {}))
              (self: super: {
                sydtest = unmarkBroken (dontCheck super.sydtest);
                til =
                  self.generateOptparseApplicativeCompletions
                  ["til"]
                  (self.callCabal2nix "til" filteredSrc {});
              });
          });
        };
    }
    // flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = pkgsFor system;
        precommitCheck = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            actionlint.enable = true;
            hlint.enable = true;
            hpack.enable = true;
            markdownlint.enable = true;
            nil.enable = true;
            alejandra.enable = true;
            ormolu.enable = true;
            statix.enable = true;
          };
        };
      in rec {
        packages.til = pkgs.haskellPackages.til;
        packages.default = packages.til;

        apps.til = flake-utils.lib.mkApp {
          drv = pkgs.haskell.lib.justStaticExecutables (
            packages.til.overrideAttrs (oldAttrs: {
              configureFlags = oldAttrs.configureFlags ++ ["--ghc-option=-O2"];
            })
          );
        };
        apps.default = apps.til;

        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [p.til];
          buildInputs = with pkgs; [
            actionlint
            cabal-install
            ghcid
            haskell-language-server
            hlint
            mdr
            nil
            alejandra
            ormolu
            statix
            inputs.feedback.packages.${system}.default
          ];
          inherit (precommitCheck) shellHook;
        };

        checks = {pre-commit-check = precommitCheck;};
      }
    );
  nixConfig = {
    extra-substituters = "https://opensource.cachix.org";
    extra-trusted-public-keys = "opensource.cachix.org-1:6t9YnrHI+t4lUilDKP2sNvmFA9LCKdShfrtwPqj2vKc=";
  };
}
