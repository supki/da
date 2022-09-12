{ mkDerivation, aeson, base, base64-bytestring, bytestring, Cabal
, cryptonite, directory, envparse, filepath, http-client
, http-client-tls, lib, optparse-applicative, pem, process, text
, x509
}:
mkDerivation {
  pname = "dazu";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal directory filepath process ];
  executableHaskellDepends = [
    aeson base base64-bytestring bytestring cryptonite envparse
    http-client http-client-tls optparse-applicative pem text x509
  ];
  homepage = "https://budueba.com/dazu";
  description = "Generate verifiably random passwords";
  license = lib.licenses.bsd2;
  mainProgram = "dazu";
}
