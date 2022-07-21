{ project = import ./.;
  shell = (import ./shell.nix).inputDerivation;
}
