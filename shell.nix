{ pkgs ? import <nixpkgs> { }
, ghc ? pkgs.haskell.compiler.ghc902
, stack ? pkgs.stack
}:

pkgs.mkShell rec {
  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;

  buildInputs = with pkgs; [
    ghc
    glibcLocales
    gmp
    stack
    zlib
  ];

  shellHook = ''
  '';
}
