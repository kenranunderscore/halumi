{
  description = "A very basic Haskell project flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    hscurses = {
      url = "github:skogsbaer/hscurses";
      flake = false;
    };
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-parts,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
        "aarch64-linux"
      ];

      perSystem =
        { system, ... }:
        let
          ghc = "ghc910";
          pkgs = import nixpkgs {
            inherit system;
            config.allowBroken = true;
            overlays = [
              (final: prev: {
                hspkgs = prev.haskell.packages.${ghc}.override (old: {
                  overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
                    hfinal: hprev: {
                      hscurses = hfinal.callCabal2nix "hscurses" inputs.hscurses {
                        inherit (final) ncurses;
                      };
                      halumi = hfinal.callCabal2nix "halumi" (final.lib.cleanSource ./.) { };
                      h-raylib = pkgs.haskell.lib.compose.doJailbreak hprev.h-raylib;
                    }
                  );
                });
              })
            ];
          };
        in
        {
          packages.default = pkgs.hspkgs.halumi;

          devShells.default = pkgs.hspkgs.shellFor {
            packages = p: [ p.halumi ];
            nativeBuildInputs = [
              pkgs.hspkgs.cabal-install
              pkgs.hspkgs.cabal-fmt
              pkgs.hspkgs.haskell-language-server
              pkgs.raylib
            ];
          };
        };
    };
}
