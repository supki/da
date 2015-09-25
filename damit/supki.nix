{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }: let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages(ps: [
    ps.hdevtools ps.doctest ps.hspec-discover ps.hlint ps.ghc-mod ps.envparse pkg
  ]);
  pkg = (import ./default.nix { inherit nixpkgs compiler; });
in
  pkgs.stdenv.mkDerivation rec {
    name = "supki";
    buildInputs = [ ghc ];
  }
