let
    pin = import ./chr/pin.nix;
in
{
    pkgs ? pin.nixpkgs,
    withGhcjs ? false,
    misoPkgs ? pin.misoNixpkgs,
}
:
if withGhcjs
then misoPkgs.haskell.packages.ghcjs.callCabal2nix "bem" ./. {}
else misoPkgs.haskell.packages.ghc864.callCabal2nix "bem" ./. {}
