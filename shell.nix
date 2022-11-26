let
    miso = pin.miso {};
    pin = import ./chr/pin.nix;
    traverse = pin.traverse {};
    unpath = pin.unpath {};
in
{
    ms ? miso,
}
:
ms.pkgs.mkShell
    {
        buildInputs = [ms.pkgs.cabal-install traverse unpath];
        inputsFrom
            =
            [(ms.pkgs.haskell.packages.ghc865.callCabal2nix "bem" ./. {}).env];
    }
