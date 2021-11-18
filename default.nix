{ mkDerivation, aeson, base, base16, bytestring, cryptohash-sha256
, esqueleto, exceptions, filepath, http-types, lib, monad-logger
, mtl, optparse-applicative, persistent, persistent-postgresql
, resource-pool, servant, servant-multipart, servant-server, text
, time, wai, wai-extra, wai-logger, warp
}:
mkDerivation {
  pname = "nft-marketplace-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base base16 bytestring cryptohash-sha256 esqueleto exceptions
    filepath http-types monad-logger mtl optparse-applicative
    persistent persistent-postgresql resource-pool servant
    servant-multipart servant-server text time wai wai-extra wai-logger
    warp
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
