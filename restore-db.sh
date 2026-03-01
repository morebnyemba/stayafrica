#!/bin/bash
# StayAfrica Database Restore Script
# Usage: ./restore-db.sh <backup_file> [--confirm]

set -euo pipefail

BACKUP_FILE="${1:-}"
CONFIRM="${2:-}"

DB_NAME="${DB_NAME:-stayafrica_db}"
DB_USER="${DB_USER:-stayafrica_user}"
DB_HOST="${DB_HOST:-db}"
DB_PORT="${DB_PORT:-5432}"

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

if [ -z "${BACKUP_FILE}" ]; then
    echo "Usage: ./restore-db.sh <backup_file.sql.gz> [--confirm]"
    echo ""
    echo "Available backups:"
    ls -lht /var/backups/stayafrica/daily/ /var/backups/stayafrica/weekly/ /var/backups/stayafrica/manual/ 2>/dev/null | head -20
    exit 1
fi

if [ ! -f "${BACKUP_FILE}" ]; then
    # Try S3
    if [ -n "${S3_BACKUP_BUCKET:-}" ]; then
        log "File not found locally, trying S3..."
        LOCAL_FILE="/tmp/$(basename ${BACKUP_FILE})"
        aws s3 cp "s3://${S3_BACKUP_BUCKET}/stayafrica-backups/${BACKUP_FILE}" "${LOCAL_FILE}"
        BACKUP_FILE="${LOCAL_FILE}"
    else
        log "ERROR: Backup file not found: ${BACKUP_FILE}"
        exit 1
    fi
fi

log "=== DATABASE RESTORE ==="
log "Backup:   ${BACKUP_FILE}"
log "Target:   ${DB_NAME}@${DB_HOST}:${DB_PORT}"
log "========================"

if [ "${CONFIRM}" != "--confirm" ]; then
    echo ""
    echo "WARNING: This will REPLACE ALL DATA in ${DB_NAME}!"
    echo "Run with --confirm flag to proceed:"
    echo "  ./restore-db.sh ${BACKUP_FILE} --confirm"
    exit 1
fi

# Create pre-restore backup
log "Creating pre-restore safety backup..."
SAFETY_BACKUP="/var/backups/stayafrica/manual/pre_restore_$(date +%Y%m%d_%H%M%S).sql.gz"
mkdir -p /var/backups/stayafrica/manual
PGPASSWORD="${DB_PASSWORD}" pg_dump \
    -h "${DB_HOST}" -p "${DB_PORT}" -U "${DB_USER}" -d "${DB_NAME}" \
    --format=custom --compress=9 2>/dev/null | gzip > "${SAFETY_BACKUP}" || true
log "Safety backup: ${SAFETY_BACKUP}"

# Restore
log "Restoring database from backup..."
gunzip -c "${BACKUP_FILE}" | PGPASSWORD="${DB_PASSWORD}" pg_restore \
    -h "${DB_HOST}" \
    -p "${DB_PORT}" \
    -U "${DB_USER}" \
    -d "${DB_NAME}" \
    --clean \
    --if-exists \
    --no-owner \
    --no-privileges \
    --single-transaction \
    --verbose 2>/tmp/pg_restore.log

log "Restore complete!"

# Verify
TABLE_COUNT=$(PGPASSWORD="${DB_PASSWORD}" psql -h "${DB_HOST}" -p "${DB_PORT}" -U "${DB_USER}" -d "${DB_NAME}" -t -c "SELECT count(*) FROM information_schema.tables WHERE table_schema = 'public';" 2>/dev/null)
log "Verified: ${TABLE_COUNT} tables in restored database"

log "Database restore from ${BACKUP_FILE} completed successfully"
