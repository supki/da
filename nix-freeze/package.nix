{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "nix-freeze";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ base ];
  homepage = "https://github.com/supki/da";
  description = "Freeze Nix environment into a Cabal constraints file";
  license = stdenv.lib.licenses.bsd2;
}
