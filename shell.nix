let
    pin = import ./chr/pin.nix;
in
{
    pkgs ? pin.nixpkgs,
}
:
pkgs.mkShell
    {
        buildInputs = [pkgs.cabal-install];
        inputsFrom
            =
            [(pkgs.haskell.packages.ghc864.callCabal2nix "bem" ./. {}).env];
    }
