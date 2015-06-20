{ mkDerivation, base, bytestring, conduit, conduit-extra, directory
, envparse, filepath, http-client, http-client-tls, lens, resourcet
, stdenv, text, time, transformers, xml-html-conduit-lens
}:
mkDerivation {
  pname = "darauf";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    base bytestring conduit conduit-extra directory envparse filepath
    http-client http-client-tls lens resourcet text time transformers
    xml-html-conduit-lens
  ];
  homepage = "https://github.com/supki/da";
  description = "Utility for querying Nix channels' status";
  license = stdenv.lib.licenses.bsd2;
}
