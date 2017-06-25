{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.callPackage ./nixos-multi-spawn.nix {}
