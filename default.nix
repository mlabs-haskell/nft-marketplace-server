{ mkDerivation, aeson, base, base16, bytestring, conduit
, cryptohash-sha256, directory, esqueleto, exceptions, filepath
, http-client, http-client-tls, http-media, http-types, ipfs-api
, lib, monad-logger, mtl, optparse-applicative, persistent
, persistent-pagination, persistent-postgresql, resource-pool
, servant, servant-client, servant-multipart, servant-pagination
, servant-server, text, time, unordered-containers, wai, wai-cors
, wai-extra, wai-logger, warp
}:
mkDerivation {
  pname = "nft-marketplace-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base base16 bytestring conduit cryptohash-sha256 directory
    esqueleto exceptions filepath http-client http-client-tls
    http-media http-types ipfs-api monad-logger mtl
    optparse-applicative persistent persistent-pagination
    persistent-postgresql resource-pool servant servant-client
    servant-multipart servant-pagination servant-server text time
    unordered-containers wai wai-cors wai-extra wai-logger warp
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
