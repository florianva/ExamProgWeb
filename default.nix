{ pkgs ? import <nixpkgs> {} }:
let
  _drv = pkgs.haskellPackages.callCabal2nix "mytracker" ./. {};
in
  if pkgs.lib.inNixShell then _drv.env else _drv
