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
        with final.haskell.lib; rec {
          til = final.haskell.lib.justStaticExecutables (
            haskellPackages.til.overrideAttrs (oldAttrs: {
              configureFlags = oldAttrs.configureFlags ++ ["--ghc-option=-O2"];
            })
          );
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
      homeManagerModules.default = {
        config,
        pkgs,
        lib,
        ...
      }:
        with lib; let
          cfg = config.programs.til;
        in {
          options.programs.til = {
            enable = mkEnableOption "til";
            editor = mkOption {
              type = types.str;
              description = "The text editor to open markdown files.";
              default = "$EDITOR";
            };
            directory = mkOption {
              type = types.str;
              description = "The directory containing the log markdown files";
              default = "$HOME/til";
            };
          };
          config = let
            til-app = self.apps.${pkgs.system}.til.program;
            til = pkgs.writeShellScriptBin "til" ''
              ${til-app} --editor ${cfg.editor} --directory ${cfg.directory} $@
            '';
          in
            mkIf cfg.enable {home.packages = [til];};
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

        apps.til = flake-utils.lib.mkApp {drv = pkgs.til;};
        apps.default = apps.til;

        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [p.til];
          buildInputs = with pkgs; [
            actionlint
            cabal-install
            ghcid
            haskell-language-server
            haskellPackages.implicit-hie
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
