#!/usr/bin/env sh
# This script ensures the Postgres password matches what the backend expects.
# Run this INSIDE the DB container on first boot via docker-entrypoint-initdb.d
# or manually after password mismatches.

set -e

# Expected password from environment (set in docker-compose)
EXPECTED_PASSWORD="${POSTGRES_PASSWORD:-postgres}"

echo "Ensuring postgres user password is set to environment value..."

# This ALTER runs as the postgres superuser during init
psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "$POSTGRES_DB" <<-EOSQL
    -- Idempotent: always set the password to match env
    ALTER USER postgres WITH PASSWORD '$EXPECTED_PASSWORD';
    
    -- Optional: Create a dedicated app user if DB_APP_USER is set
    -- Uncomment below if you want a non-superuser app role:
    -- DO \$\$
    -- BEGIN
    --     IF NOT EXISTS (SELECT FROM pg_catalog.pg_roles WHERE rolname = '${DB_APP_USER}') THEN
    --         CREATE USER ${DB_APP_USER} WITH PASSWORD '${DB_APP_PASSWORD}';
    --     END IF;
    -- END \$\$;
    -- GRANT ALL PRIVILEGES ON DATABASE ${POSTGRES_DB} TO ${DB_APP_USER};
EOSQL

echo "Password sync complete."
