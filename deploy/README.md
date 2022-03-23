# nft-marketplace-server deployment

This document describes current deployment of `nft-marketplace-server` on AWS instance.

At the moment deployment assumes deploying Postgres in Docker container, then building `nft-marketplace-server` with `nix` and starting/restarting corresponding `systemd` service.

For NFT data storage `nft.storage` is used.

## Requirements
To be able to deploy system should have:

- `bash`
- `docker` and `docker-compose`
- `nix`

## Current deployment stricture

```none
/home/ubuntu/seabug <- parent dir
├── docker_vols     <- mounted volumes for containers
├── images          <- images dir specified at `nft-marketplace-server` start
└── nft-marketplace-server <- nft-marketplace-server cloned repo
```

## Procedure

(all commands meant to be executed from `nft-marketplace-server/deploy`)

### Initial deploy

Executing steps below will result in:

- Postgres running on port `5432`
- `nft-marketplace-server` running on port `8008`

Steps:

1. Install tools listed in `Requirements`
2. Prepare directories and clone `nft-marketplace-server`
3. Deploy container with Postgres (see [compose file](docker-compose.yml))
   1. Set required environment variables
      - `SERVICES_VOL` -- directory where IPFS and Postgres data will be stored (current deployment uses `/home/ubuntu/seabug/docker_volumes`)
      - `PG_ADMIN_PASS` -- password for Postgres container superuser (username is default - `postgres`)
      - `PG_SBUG_USER` and `PG_SBUG_PASS` -- username and password for `nft-marketplace-server` database. During initial deploy user with `PG_SBUG_USER` will be created with specified password, the database with the same name as `PG_SBUG_USER` will be created and all privileges on it will be granted to created user (see [1_init_seabaug_db.sh](pg_init/1_init_sebaug_db.sh)).
   2. Run deployment script: `./deploy-services-aws.sh up -d` (see [script](./deploy-services-aws.sh) comments for other options)
   3. Wait till Postgres is ready to accept connections (e.g. check logs with `./deploy-services-aws.sh logs` or `./deploy-services-aws.sh logs -f`)
4. Deploy `nft-marketplace-server`
   1. Create `systemd` service from [nft-marketplace-server.service](nft-marketplace-server.service)
   2. Set required environment variables with `systemctl edit nft-marketplace-server.service`. Required variables are `PG_SBUG_USER`, `PG_SBUG_PASS` and `NFT_STORAGE_KEY`
   3. Run deployment script: `./deploy-server-aws.sh` ([script](deploy-server-aws.sh))
   4. Verify server started as expected with `systemctl status nft-marketplace-server.service` or `journalctl -u nft-marketplace-server.service`

### Update and redeploy

To update and restart `nft-marketplace-server`:

- run `git pull`
- run deployment script `./deploy-server-aws.sh` ([script](deploy-server-aws.sh))

To update and restart containers:

- make desired changes in [docker-compose.yml](docker-compose.yml)
- run deployment script: `./deploy-services-aws.sh up -d` ([script](./deploy-services-aws.sh))

### Teardown

Stop `nft-marketplace-server`: `(sudo) systemctl stop nft-marketplace-server.service`

Stop containers: `./deploy-services-aws.sh stop`

OR

Stop and remove containers and created networks: `./deploy-services-aws.sh down`
(it's safe to remove containers, as all data stored on host)

## Deployment variants

Two deployment variants are available now depending on how to organize NFT data storage:

- use `nft.storage` with
  - `deploy-services-aws.sh`
  - `deploy-server-aws.sh`
  - `nft-marketplace-server.service`
- deploy own IPFS node with Docker with
  - `deploy-services.sh`
  - `deploy-server.sh`

Current AWS deployment described above uses `nft.storage` variant.

Variant with local IPFS node can be used as fast way to deploy things locally for debugging, e.g..

## Docker settings

To avoid networks overlapping and uncontrollable space consumption by logs `/etc/docker/daemon.json` with following content was created:

```json
{
  "bip": "10.200.0.1/24",
  "default-address-pools":[
    {"base":"10.201.0.0/16","size":24},
    {"base":"10.202.0.0/16","size":24}
  ],
  "log-opts": {
    "max-size": "100m",
    "max-file": "30"
  }
}
```

## Some resources

- [Install Docker Engine on Ubuntu](https://docs.docker.com/engine/install/ubuntu/)
- [Setting up an IPFS Node](https://willschenk.com/articles/2019/setting_up_an_ipfs_node/)

## Possible enhancements

- Automation of initial deployment by merging containers and server deployment scripts, some mechanism for awaiting containers to be ready required (e.g. [wait-for-it](https://github.com/vishnubob/wait-for-it) or alike)
- Database backups
- Better database config (default one from container used now)
- Status monitoring (down detection at least)
- CI/CD
