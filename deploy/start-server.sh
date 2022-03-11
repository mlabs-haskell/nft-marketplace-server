#!/bin/bash

# Start NFT-server

set -euo pipefail

./nft-marketplace-server \
  -p 8008 \
  --db-connection "postgresql://seadebug:seadebugpass@localhost:5432/seadebug" \
  --ipfs-node "localhost:8081" \
  --image-folder "./images"
