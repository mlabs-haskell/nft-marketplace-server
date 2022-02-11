{ mkDerivation, aeson, base, base16, bytestring, conduit
, cryptohash-sha256, directory, esqueleto, exceptions, filepath
, http-types, ipfs, lib, monad-logger, mtl, optparse-applicative
, persistent, persistent-pagination, persistent-postgresql
, resource-pool, servant, servant-client, servant-multipart
, servant-pagination, servant-server, text, time, wai, wai-extra
, wai-logger, warp
}:
mkDerivation {
  pname = "nft-marketplace-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base base16 bytestring conduit cryptohash-sha256 directory
    esqueleto exceptions filepath http-types ipfs monad-logger mtl
    optparse-applicative persistent persistent-pagination
    persistent-postgresql resource-pool servant servant-client
    servant-multipart servant-pagination servant-server text time wai
    wai-extra wai-logger warp
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
