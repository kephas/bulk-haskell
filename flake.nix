{
  description = "bulk-haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        hpkgs = pkgs.haskellPackages;
      in
      {
        defaultPackage = (hpkgs.callCabal2nix "bulk-haskell" ./. { });

        devShell = pkgs.mkShell {
          buildInputs = with hpkgs; [
            cabal-install
            hpack
            haskell-language-server
            fourmolu
            ghcid
            pkgs.just
          ];
          inputsFrom = [ self.defaultPackage.${system}.env ];
        };
      }
    );
}
