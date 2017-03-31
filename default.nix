{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.callPackage ./nix-nspawn-run.nix {}
