{ pkgs ? import <nixpkgs> {} }:
let
  dap = pkgs.haskellPackages.callCabal2nix "dap" ./. {};
in
{
  inherit dap pkgs;
}
