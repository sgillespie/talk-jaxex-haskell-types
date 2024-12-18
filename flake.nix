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
          devShell.tools = hp: with hp; {
            inherit feedback;
            inherit (pkgs) pandoc;
          };
          autoWire = [ "packages" "apps" "checks" "devShells" ];
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

        packages = {
          default = self'.packages.haskell-types-examples;
          build-slides =
            let
              drv = {
                src = ./slides;
                nativeBuildInputs = [ pkgs.pandoc ];
              };
              s5 = pkgs.fetchzip {
                url = "https://meyerweb.com/eric/tools/s5/v/1.1/s5-11.zip";
                hash = "sha256-yTm7GuPskITCxpOOWCFAoKG28TRoULka1n+pYBtlMAY=";
                stripRoot = false;
              };
            in
              pkgs.runCommand "build-slides" drv ''
                mkdir -p $out
                cp -r ${s5}/ui $out/s5
                pandoc -t s5 -s $src/slides.md -o $out/index.html
              '';
        };
      };
    };
}
