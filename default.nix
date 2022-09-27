let
  reflex-platform = import ./reflex-platform { };

in
{
  seven-guis-vty = reflex-platform.ghc.callCabal2nix "seven-guis-vty-counter" ./. { };
}
