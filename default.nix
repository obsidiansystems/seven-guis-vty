let
  reflex-platform = import ./reflex-platform { };
  callCabal2nix = reflex-platform.ghc.callCabal2nix;
  overrideCabal = reflex-platform.overrideCabal;
  # Only build the given executable target
  buildOnlyExecutable = target: drv: overrideCabal drv {
    buildTarget = target;
    isExecutable = true;
  };
in
{
  seven-guis-vty = callCabal2nix "seven-guis-vty" ./. { };
  seven-guis-vty-counter = buildOnlyExecutable "seven-guis-vty-counter"
    (callCabal2nix "seven-guis-vty" ./. {});
  seven-guis-vty-temperature-converter = buildOnlyExecutable "seven-guis-vty-temperature-converter"
    (callCabal2nix "seven-guis-vty" ./. {});
}
