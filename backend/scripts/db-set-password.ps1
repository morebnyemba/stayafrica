param(
    [string]$NewPassword = "postgres",
    [string]$ContainerName = "stayafrica_db"
)

Write-Host "Updating Postgres password in container '$ContainerName'..."

# This alters the existing 'postgres' user password inside the DB without recreating the volume
$cmd = "psql -U postgres -c \"ALTER USER postgres WITH PASSWORD '$NewPassword';\""

try {
    docker exec -i $ContainerName sh -c $cmd
    Write-Host "Password updated successfully." -ForegroundColor Green
}
catch {
    Write-Host "Failed to update password. Ensure the container '$ContainerName' is running." -ForegroundColor Red
    Write-Host $_.Exception.Message
    exit 1
}

Write-Host "Restarting backend services to pick up the new password..."
try {
    docker compose up -d backend celery celery-beat
    Write-Host "Services restarted." -ForegroundColor Green
}
catch {
    Write-Host "Failed to restart services. You can run: docker compose up -d backend celery celery-beat" -ForegroundColor Yellow
}