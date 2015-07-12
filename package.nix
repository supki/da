{ mkDerivation, base, filepath, hspec, stdenv, transformers, unix
}:
mkDerivation {
  pname = "damit";
  version = "0.1.0";
  src = ./.;
  buildDepends = [ base filepath transformers unix ];
  testDepends = [ base hspec ];
  homepage = "https://supki.github.io/damit";
  description = "Yet another command launcher";
  license = stdenv.lib.licenses.bsd2;
}
