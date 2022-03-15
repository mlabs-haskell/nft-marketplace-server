#!/bin/bash

# Deploy NFT-server on AWS from repo

set -euo pipefail
cd ..
. ~/.nix-profile/etc/profile.d/nix.sh
nix build -f release.nix --extra-experimental-features nix-command

systemctl restart nft-marketplace-server.service