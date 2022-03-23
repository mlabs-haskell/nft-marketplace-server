#!/bin/bash

# Deploy NFT-server from repo
# Requires systemd service to be configured first  (see README.md)

set -euo pipefail
cd ..
. ~/.nix-profile/etc/profile.d/nix.sh
nix build -f release.nix --extra-experimental-features nix-command

sudo systemctl enable nft-marketplace-server.service
sudo systemctl restart nft-marketplace-server.service
