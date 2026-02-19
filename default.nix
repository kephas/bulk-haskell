{ pkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs/archive/refs/heads/nixos-25.11.tar.gz) {} }:

pkgs.haskellPackages.developPackage {
  root = ./.;
  modifier = (pkgs.haskell.lib.compose.addBuildTools (with pkgs; [ cabal-install hpack haskell-language-server just ghcid hlint ]));
}
