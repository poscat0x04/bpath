{
  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
  inputs.flake-utils.url = github:poscat0x04/flake-utils;

  outputs = { self, nixpkgs, flake-utils, ... }: with flake-utils;
    eachDefaultSystem (
      system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
        in
          with pkgs;
          {
            devShell = bpath-dev.envFunc { withHoogle = true; };
            defaultPackage = bpath;
          }
    ) // {
      overlay = self: super:
        let
          hpkgs = super.haskellPackages;
          bpath = hpkgs.callCabal2nix "bpath" ./. {};
        in
          with super; with haskell.lib;
          {
            inherit bpath;
            bpath-dev = addBuildTools bpath [
              haskell-language-server
              cabal-install
            ];
          };
    };
}
