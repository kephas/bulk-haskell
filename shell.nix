#{ pkgs ? import <nixpkgs> {} }:
{ pkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs/archive/4d2b37a84fad1091b9de401eb450aae66f1a741e.tar.gz) {} }: # 22.11

pkgs.mkShell {
  packages = [pkgs.haskell-language-server (pkgs.haskellPackages.ghcWithPackages (p: [p.binary p.binary-parsers p.bytestring p.hspec p.largeword p.QuickCheck p.random p.sandwich])) ];
}
