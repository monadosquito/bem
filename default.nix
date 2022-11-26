let
    miso = pin.miso {};
    pin = import ./chr/pin.nix;
in
{
    ms ? miso,
    useGhcjs ? false,
}
:
if useGhcjs
then ms.pkgs.haskell.packages.ghcjs.callCabal2nix "bem" ./. {}
else ms.pkgs.haskell.packages.ghc865.callCabal2nix "bem" ./. {}
