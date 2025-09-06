{ pkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs/archive/refs/tags/22.11.tar.gz) {} }:

let hp2505 = (import (fetchTarball https://github.com/NixOS/nixpkgs/archive/refs/tags/25.05.tar.gz) {}).haskellPackages;
    hp = pkgs.haskellPackages.override { overrides = self: super: { digits = hp2505.digits; }; };
    hlib = pkgs.haskell.lib.compose;
    compose = with pkgs.lib.trivial; flip pipe;
in

# missing dev.haskellPackages.digits
hp.developPackage {
  root = ./.;
  modifier = compose [(hlib.addExtraLibrary hp.shake)
                      (hlib.addBuildTools (with pkgs; [ shake haskell-language-server hlint ]))];
}
