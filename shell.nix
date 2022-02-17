{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

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

  _nixpkgs = import (nixpkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "2cf9db0e3d45b9d00f16f2836cb1297bcadc475e";
    sha256 = "0sij1a5hlbigwcgx10dkw6mdbjva40wzz4scn0wchv7yyi9ph48l";
  }) { overlays = [ servant-pagination-overlay ]; };

  pkgs = _nixpkgs.pkgs;

  f = { mkDerivation, aeson, base, base16, bytestring
      , cryptohash-sha256, directory, esqueleto, exceptions, filepath, http-types
      , lib, monad-logger, mtl, optparse-applicative, persistent
      , persistent-pagination, persistent-postgresql, resource-pool
      , servant, servant-multipart, servant-pagination, servant-server
      , text, time, wai, wai-extra, wai-logger, warp, servant-client, ipfs-api, wai-cors
      }:
      mkDerivation {
        pname = "nft-marketplace-server";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base base16 bytestring cryptohash-sha256 directory esqueleto exceptions
          filepath http-types monad-logger mtl optparse-applicative
          persistent persistent-pagination persistent-postgresql
          resource-pool servant servant-multipart servant-pagination
          servant-server text time wai wai-extra wai-logger warp servant-client
          ipfs-api wai-cors
        ];
        license = "unknown";
        hydraPlatforms = lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
