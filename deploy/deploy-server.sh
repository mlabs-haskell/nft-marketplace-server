#!/bin/bash

# Helper to start NFT-server

set -euo pipefail

# Required environment variables (with examples)
# export PG_SBUG_USER=seadebug
# export PG_SBUG_PASS=654321

# E.g.: ./deploy-server.sh --image-folder /home/ubuntu/seabug/images
cd ..
. ~/.nix-profile/etc/profile.d/nix.sh
nix build -f release.nix --extra-experimental-features nix-command

./result/bin/nft-marketplace-server \
  -p 8008 \
  --db-connection "postgresql://$PG_SBUG_USER:$PG_SBUG_PASS@localhost:5432/$PG_SBUG_USER" \
  --ipfs-node "localhost:5001"\
  "$@" # for specifying images directory