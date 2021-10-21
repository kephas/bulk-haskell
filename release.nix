{ nixpkgs ? import ./nixpkgs.nix {}
}:
let
  inherit (nixpkgs)
    callPackage
    dockerTools
    busybox;

  package = callPackage ./. {};
in
  dockerTools.buildImage {
    name = "duncan";
    tag = "latest";
    contents = [
      package
      busybox
    ];
    config = {
      Cmd = ["/bin/${package.pname}"];
    };
  }
