let
  reflex-platform = import ./reflex-platform { };
  ghc = reflex-platform.ghc.override {
    overrides = self: super: {
      reflex-vty = self.callHackageDirect {
        pkg = "reflex-vty";
        ver = "0.3.1.0";
        sha256 = "0nm4wiy03b19r3lgv7zzybs1bdlsk9lhfhvd0l7c386lkz9bm2yh";
      } {};
    };
  };
  callCabal2nix = ghc.callCabal2nix;
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
