{ nixpkgs ? import <nixpkgs> {} }:

let _nixpkgs = import (nixpkgs.fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs";
  rev = "2cf9db0e3d45b9d00f16f2836cb1297bcadc475e";
  sha256 = "0sij1a5hlbigwcgx10dkw6mdbjva40wzz4scn0wchv7yyi9ph48l";
}) {};
in

with _nixpkgs;
{
  nft-marketplace-server = haskellPackages.callPackage ./default.nix {};
}
