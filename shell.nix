let
  platform = import ./reflex-platform {};
  pkgs = platform.nixpkgs;
in
  pkgs.mkShell {
    name = "seven-guis-vty-counter";
    buildInputs = [
      platform.ghc.ghc
      pkgs.ghcid
      pkgs.cabal-install
      platform.ghc.markdown-unlit
    ];
    inputsFrom = [
      (import ./.).env
    ];
  }
