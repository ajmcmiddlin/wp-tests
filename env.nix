{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  inherit (nixpkgs) pkgs;

  haskellPackages =
    (if compiler == "default"
       then pkgs.haskellPackages
       else pkgs.haskell.packages.${compiler}).override {
          overrides = self: super: {
            tasty-hedgehog = self.callHackage "tasty-hedgehog" "0.2.0.0" {};
          };
       };
in
  {
    inherit haskellPackages pkgs;
    drv = haskellPackages.callPackage ./wp-test.nix {};
  }
