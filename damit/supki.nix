{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }: let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages(ps: [
    ps.hdevtools ps.doctest ps.hspec-discover ps.envparse pkg
  ]);
  pkg = (import ./default.nix { inherit nixpkgs compiler; });
in
  pkgs.stdenv.mkDerivation rec {
    name = "supki";
    buildInputs = [ ghc ];
  }
