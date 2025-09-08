{ pkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs/archive/67d0da2b164cd4301464d7037f041f65238fd8e3.tar.gz) {} }:

pkgs.haskellPackages.developPackage {
  root = ./.;
  modifier = (pkgs.haskell.lib.compose.addBuildTools (with pkgs; [ cabal-install hpack haskell-language-server hlint ]));
}
