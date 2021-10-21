# { pkgs ? import <nixpkgs> {}
# , haskellPackages ? pkgs.haskellPackages
# , withHoogle ? true }:

# let overrideCabal = pkg: pkgs.haskell.lib.overrideCabal pkg ({ buildDepends ? [], ... }: {
#       buildDepends = buildDepends ++ [ pkgs.cabal-install ];
#     });

# in (overrideCabal (import ./default.nix { inherit pkgs haskellPackages withHoogle; })).env

with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/c4ed2a618c5f8b51c1be5983c9c2e662d76679ad.tar.gz) {};
let ghc = haskell.compiler.ghc8106; # resolver: lts-18.8
in haskell.lib.buildStackProject {
    inherit ghc;
    name = "duncan";
    buildInputs = [ zlib ];
    src = ./.;
}
