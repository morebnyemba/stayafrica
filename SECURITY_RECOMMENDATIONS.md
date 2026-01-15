# Security Recommendations for StayAfrica

## Database Credentials

### Current State
Currently, database credentials are hardcoded in `docker-compose.prod.yml` for all services:
- `db` service: `POSTGRES_USER: postgres`, `POSTGRES_PASSWORD: postgres`
- `backend` service: `DATABASE_USER: postgres`, `DATABASE_PASSWORD: postgres`
- `celery` service: `DATABASE_USER: postgres`, `DATABASE_PASSWORD: postgres`
- `celery-beat` service: `DATABASE_USER: postgres`, `DATABASE_PASSWORD: postgres`

### Security Risk
Hardcoded credentials in version-controlled files pose a security risk, especially for production deployments.

### Recommended Improvements

#### Option 1: Use Environment Variables (Easiest)
Create a `.env` file at the project root (not committed to version control):

```bash
# .env (add to .gitignore)
POSTGRES_USER=postgres
POSTGRES_PASSWORD=your_secure_password_here
```

Then update `docker-compose.prod.yml`:

```yaml
services:
  db:
    environment:
      POSTGRES_USER: ${POSTGRES_USER}
      POSTGRES_PASSWORD: ${POSTGRES_PASSWORD}
      
  backend:
    environment:
      DATABASE_USER: ${POSTGRES_USER}
      DATABASE_PASSWORD: ${POSTGRES_PASSWORD}
      
  celery:
    environment:
      DATABASE_USER: ${POSTGRES_USER}
      DATABASE_PASSWORD: ${POSTGRES_PASSWORD}
      
  celery-beat:
    environment:
      DATABASE_USER: ${POSTGRES_USER}
      DATABASE_PASSWORD: ${POSTGRES_PASSWORD}
```

#### Option 2: Use Docker Secrets (Most Secure)
For production environments, use Docker Swarm secrets:

```yaml
secrets:
  db_password:
    external: true

services:
  db:
    secrets:
      - db_password
    environment:
      POSTGRES_PASSWORD_FILE: /run/secrets/db_password
```

#### Option 3: Use External Secret Management
Integrate with secret management systems like:
- AWS Secrets Manager
- HashiCorp Vault
- Azure Key Vault
- Google Cloud Secret Manager

### Implementation Priority
1. **Immediate**: Change the default password from `postgres` to a strong password
2. **Short-term**: Move credentials to `.env` file (Option 1)
3. **Long-term**: Implement Docker secrets or external secret management for production

### Other Security Considerations
1. **JWT Secret Keys**: Change default JWT secret keys in production
2. **Django Secret Key**: Generate a unique secret key for production
3. **SSL/TLS**: Ensure all connections use HTTPS in production
4. **Database Access**: Restrict database port exposure (remove `ports: - "5432:5432"` from db service)
5. **Redis Security**: ✅ **IMPLEMENTED** - Redis is now secured with:
   - Password authentication required
   - No public port exposure (removed from docker-compose files)
   - Internal Docker network access only
   - Dangerous commands disabled
   - See `REDIS_SECURITY.md` for complete details
6. **Regular Updates**: Keep all Docker images and dependencies up to date
7. **Backup Strategy**: Implement regular database backups
8. **Monitoring**: Set up security monitoring and alerts

### Current Fix
The recent changes to fix Celery worker authentication maintain consistency with the existing credential management approach. While not ideal for production, this ensures all services can connect to the database. Please prioritize implementing the recommended improvements before deploying to production.
