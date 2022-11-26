let
    pin = import ./chr/pin.nix;
in
{
    pkgs ? pin.nixpkgs,
    withGhcjs ? false,
}
:
if withGhcjs
then pkgs.haskell.packages.ghcjs.callCabal2nix "bem" ./. {}
else pkgs.haskell.packages.ghc864.callCabal2nix "bem" ./. {}
