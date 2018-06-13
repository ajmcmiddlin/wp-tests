{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  env = import ./env.nix {inherit nixpkgs compiler; };
in
  env.drv
