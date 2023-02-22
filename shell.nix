let
    miso = pin.miso {};
    pin = import ./chr/pin.nix;
    traverse = pin.traverse {};
    unpath = pin.unpath {};
    nixpkgs = pin.nixpkgs {};
in
{
    ms ? miso,
    pkgs ? nixpkgs,
}
:
ms.pkgs.mkShell
    {
        buildInputs
            =
            [
                traverse
                unpath
                (ms.pkgs.writeShellScriptBin
                     "watch"
                     (ms.pkgs.lib.readFile ./scr/watch.sh)
                )
                ms.pkgs.cabal-install
                pkgs.ghcid
            ];
        inputsFrom
            =
            [(ms.pkgs.haskell.packages.ghc865.callCabal2nix "bem" ./. {}).env];
    }
