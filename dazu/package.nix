{ mkDerivation, aeson, base, base64-bytestring, bytestring
, cryptonite, envparse, http-client, http-client-tls
, optparse-applicative, pem, stdenv, text, x509
}:
mkDerivation {
  pname = "dazu";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base base64-bytestring bytestring cryptonite envparse
    http-client http-client-tls optparse-applicative pem text x509
  ];
  homepage = "https://budueba.com/dazu";
  description = "Generate verifiably random passwords";
  license = stdenv.lib.licenses.bsd2;
}
