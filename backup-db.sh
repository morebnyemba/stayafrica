#!/bin/bash
# StayAfrica Database Backup Script
# Usage: ./backup-db.sh [daily|weekly|manual]
# Requires: pg_dump, gzip, optionally aws cli for S3 upload

set -euo pipefail

BACKUP_TYPE="${1:-daily}"
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
BACKUP_DIR="/var/backups/stayafrica"
S3_BUCKET="${S3_BACKUP_BUCKET:-}"
S3_PREFIX="stayafrica-backups"

# Database credentials from environment
DB_NAME="${DB_NAME:-stayafrica_db}"
DB_USER="${DB_USER:-stayafrica_user}"
DB_HOST="${DB_HOST:-db}"
DB_PORT="${DB_PORT:-5432}"

BACKUP_FILE="${BACKUP_DIR}/${BACKUP_TYPE}/stayafrica_${BACKUP_TYPE}_${TIMESTAMP}.sql.gz"

# Retention policies (in days)
DAILY_RETENTION=7
WEEKLY_RETENTION=30
MANUAL_RETENTION=90

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

mkdir -p "${BACKUP_DIR}/daily" "${BACKUP_DIR}/weekly" "${BACKUP_DIR}/manual"

log "Starting ${BACKUP_TYPE} backup of ${DB_NAME}..."

# Perform backup with pg_dump
PGPASSWORD="${DB_PASSWORD}" pg_dump \
    -h "${DB_HOST}" \
    -p "${DB_PORT}" \
    -U "${DB_USER}" \
    -d "${DB_NAME}" \
    --no-owner \
    --no-privileges \
    --format=custom \
    --compress=9 \
    --verbose \
    2>/tmp/pg_dump_${TIMESTAMP}.log \
    | gzip > "${BACKUP_FILE}"

BACKUP_SIZE=$(du -sh "${BACKUP_FILE}" | cut -f1)
log "Backup created: ${BACKUP_FILE} (${BACKUP_SIZE})"

# Verify backup integrity
if gzip -t "${BACKUP_FILE}" 2>/dev/null; then
    log "Backup integrity check: PASSED"
else
    log "ERROR: Backup integrity check FAILED!"
    exit 1
fi

# Upload to S3 if bucket is configured
if [ -n "${S3_BUCKET}" ]; then
    log "Uploading to S3: s3://${S3_BUCKET}/${S3_PREFIX}/${BACKUP_TYPE}/"
    aws s3 cp "${BACKUP_FILE}" \
        "s3://${S3_BUCKET}/${S3_PREFIX}/${BACKUP_TYPE}/" \
        --storage-class STANDARD_IA \
        --only-show-errors
    log "S3 upload complete"

    # Apply S3 lifecycle for rotation
    aws s3api put-bucket-lifecycle-configuration \
        --bucket "${S3_BUCKET}" \
        --lifecycle-configuration '{
            "Rules": [
                {
                    "ID": "daily-cleanup",
                    "Filter": {"Prefix": "'"${S3_PREFIX}"'/daily/"},
                    "Status": "Enabled",
                    "Expiration": {"Days": '"${DAILY_RETENTION}"'}
                },
                {
                    "ID": "weekly-cleanup",
                    "Filter": {"Prefix": "'"${S3_PREFIX}"'/weekly/"},
                    "Status": "Enabled",
                    "Expiration": {"Days": '"${WEEKLY_RETENTION}"'}
                },
                {
                    "ID": "manual-glacier",
                    "Filter": {"Prefix": "'"${S3_PREFIX}"'/manual/"},
                    "Status": "Enabled",
                    "Transitions": [{"Days": 30, "StorageClass": "GLACIER"}],
                    "Expiration": {"Days": 365}
                }
            ]
        }' 2>/dev/null || log "S3 lifecycle already configured"
fi

# Local rotation
case "${BACKUP_TYPE}" in
    daily)
        find "${BACKUP_DIR}/daily" -name "*.sql.gz" -mtime +${DAILY_RETENTION} -delete
        log "Cleaned daily backups older than ${DAILY_RETENTION} days"
        ;;
    weekly)
        find "${BACKUP_DIR}/weekly" -name "*.sql.gz" -mtime +${WEEKLY_RETENTION} -delete
        log "Cleaned weekly backups older than ${WEEKLY_RETENTION} days"
        ;;
    manual)
        find "${BACKUP_DIR}/manual" -name "*.sql.gz" -mtime +${MANUAL_RETENTION} -delete
        log "Cleaned manual backups older than ${MANUAL_RETENTION} days"
        ;;
esac

log "Backup complete: ${BACKUP_TYPE} backup of ${DB_NAME}"

# List recent backups
log "Recent backups:"
ls -lh "${BACKUP_DIR}/${BACKUP_TYPE}/" | tail -5
