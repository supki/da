{ pkgs ? import <nixpkgs> { }
, ghc ? pkgs.haskell.compiler.ghc965
}:

pkgs.mkShell rec {
  buildInputs = with pkgs; [
    ghc
    glibcLocales
    gmp
    stack
    zlib
  ];

  shellHook = ''
    export LANG=en_US.UTF-8 \
           LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath buildInputs}"
  '';
}
