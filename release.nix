let
  defaultNixpkgs = import
    (builtins.fetchTarball {
      url = "https://github.com/nixos/nixpkgs/tarball/2cf9db0e3d45b9d00f16f2836cb1297bcadc475e";
      sha256 = "0sij1a5hlbigwcgx10dkw6mdbjva40wzz4scn0wchv7yyi9ph48l";
    })
    { };
in

{ nixpkgs ? defaultNixpkgs }:

let
  servant-pagination-overlay = final: prev:
    {
      haskellPackages = prev.haskellPackages.override {
        overrides = hself: hsuper: {
          servant-pagination = hself.callHackageDirect {
            pkg = "servant-pagination";
            ver = "2.4.1";
            sha256 = "0rz9i8lany191vl2232pr0dsy04wqlg55vw3wy3yq67s3v4xzzjq";
          } {};
        };
      };
    };

  _nixpkgs = nixpkgs.extend servant-pagination-overlay;
in

with _nixpkgs;
{
  nft-marketplace-server = haskellPackages.callPackage ./default.nix {};
}
