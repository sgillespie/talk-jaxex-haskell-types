{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
    flake-parts = {
      url = github:hercules-ci/flake-parts;
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    haskell-flake.url = github:srid/haskell-flake;
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    fourmolu-nix.url = "github:jedimahdi/fourmolu-nix";
  };

  outputs = { flake-parts, haskell-flake, ... }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      imports = [
        haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.fourmolu-nix.flakeModule
      ];

      perSystem = { self', system, lib, config, pkgs, ... }: {
        haskellProjects.default = {
          basePackages = pkgs.haskell.packages.ghc96;

          autoWire = [ "packages" "apps" "checks" ];
        };

        treefmt.config = {
          projectRootFile = "flake.nix";

          programs.fourmolu = {
            enable = true;
            package = config.fourmolu.wrapper;
          };
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;
          programs.hlint.enable = true;
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [
            config.haskellProjects.default.devShell
            config.treefmt.build.devShell
          ];
        };
      };
    };
}
