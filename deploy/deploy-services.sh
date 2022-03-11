#!/bin/bash

# Start infrastructure required by NFT-server

# Usage examples: 
# - Start and attach co containers: ./deploy-services.sh up
# - Start in detached mode: ./deploy-services.sh up -d
# - Tear down: ./deploy-services.sh down

set -euo pipefail

mkdir -p services

export COMPOSE_PROJECT_NAME=test

# remove `--env-file` to use this exports
# export IPFS_DATA_VOL=./ipfs/data
# export PG_USER=seadebug
# export PG_PASS=seadebugpass
# export PG_DATA_VOL=./pg/data

docker compose \
  -f ./docker-compose.yml \
  --env-file ./def.env \
  "$@"

