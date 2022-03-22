#!/bin/bash

# Create seabug database and user
set -euo pipefail

#!/bin/bash
set -e

psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" <<-EOSQL
    CREATE USER $PG_SBUG_USER WITH PASSWORD '$PG_SBUG_PASS';
    CREATE DATABASE $PG_SBUG_USER;
    GRANT ALL PRIVILEGES ON DATABASE "$PG_SBUG_USER" to $PG_SBUG_USER;
EOSQL
