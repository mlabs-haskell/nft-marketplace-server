# WIP: Basic manual deployemnt

Docker compose required.

To start:

1. Set required environment variables: `SERVICES_VOL`, `PG_ADMIN_PASS`, `PG_SBUG_USER`, `PG_SBUG_PASS`
2. Start required infrastructure with [deploy-services.sh](./deploy-services.sh)
3. Wait till DB and IPFS ready to accept connections (e.g. check logs with `./deploy-services.sh logs` or `./deploy-services.sh logs -f`)
4. Start server with [start-server.sh](./start-server.sh)

## Some resources

[Setting up an IPFS Node](https://willschenk.com/articles/2019/setting_up_an_ipfs_node/)