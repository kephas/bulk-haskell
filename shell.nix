
#{ pkgs ? import <nixpkgs> {} }:
{ pkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs/archive/refs/tags/22.11.tar.gz) {} }:

let dev = pkgs.callPackage ~/Logiciel/nixpkgs {};
in

pkgs.mkShell {
  packages = [pkgs.haskell-language-server (pkgs.haskellPackages.ghcWithPackages (p: [p.binary p.binary-parsers p.bytestring dev.haskellPackages.digits p.hspec p.largeword p.quickcheck-instances p.QuickCheck p.random p.sandwich])) ];
}
