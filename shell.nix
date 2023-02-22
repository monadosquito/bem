let
    pin = import ./chr/pin.nix;
in
{
    pkgs ? pin.nixpkgs,
    misoPkgs ? pin.misoNixpkgs,
}
:
pkgs.mkShell
    {
        buildInputs
            =
            [
                (pkgs.writeShellScriptBin
                     "watch"
                     (pkgs.lib.readFile ./script/watch.sh)
                )
                misoPkgs.cabal-install
                pkgs.ghcid
            ];
        inputsFrom
            =
            [(misoPkgs.haskell.packages.ghc864.callCabal2nix "bem" ./. {}).env];
    }
