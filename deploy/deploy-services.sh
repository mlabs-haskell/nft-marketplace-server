#!/bin/bash

# Start infrastructure required by NFT-server

# Usage examples: 
# - Start and attach co containers: ./deploy-services.sh up
# - Start in detached mode: ./deploy-services.sh up -d
# - Tear down: ./deploy-services.sh down

set -euo pipefail

# mkdir -p services

export COMPOSE_PROJECT_NAME=test

# Required environment variables (with examples)
# export SERVICES_VOL=/home/ubuntu/seabug/images
# export PG_ADMIN_PASS=123456
# export PG_SBUG_USER=seadebug
# export PG_SBUG_PASS=654321

docker-compose \
  -f ./docker-compose.yml \
  "$@"

