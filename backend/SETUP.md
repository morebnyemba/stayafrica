# Backend Development Setup Guide

## Prerequisites

### Python Version
- **Required:** Python 3.11 or higher
- **Tested:** Python 3.13.9
- **Download:** https://www.python.org/downloads/

### Windows-Specific Setup

If you encounter issues installing `psycopg2-binary` on Windows:

```bash
# Option 1: Use pre-built wheel (Recommended)
pip install psycopg2 --only-binary psycopg2

# Option 2: Use conda (if available)
conda install psycopg2

# Option 3: Install from binary wheels
pip install --only-binary :all: psycopg2-binary
```

## Installation Steps

### 1. Create Virtual Environment

```bash
# Windows
python -m venv venv
venv\Scripts\activate

# macOS/Linux
python3 -m venv venv
source venv/bin/activate
```

### 2. Upgrade pip and build tools

```bash
pip install --upgrade pip setuptools wheel
```

### 3. Install Dependencies

```bash
# For core development (Recommended for Windows)
pip install -r requirements.txt

# For full development with optional packages
pip install -r requirements-dev.txt
```

### 4. Install PostgreSQL with PostGIS

**Using Docker (Recommended):**
```bash
docker run --name stayafrica-db \
  -e POSTGRES_PASSWORD=postgres \
  -e POSTGRES_DB=stayafrica \
  -p 5432:5432 \
  -v pgdata:/var/lib/postgresql/data \
  postgis/postgis:15
```

**Or use docker-compose:**
```bash
docker-compose up -d db
```

### 5. Create Environment File

```bash
cp .env.example .env
```

Edit `.env` with your configuration:
```env
DEBUG=True
SECRET_KEY=your-secret-key-here
DATABASE_URL=postgresql://postgres:postgres@localhost:5432/stayafrica
ALLOWED_HOSTS=localhost,127.0.0.1
```

### 6. Run Migrations

```bash
python manage.py migrate
```

### 7. Create Superuser

```bash
python manage.py createsuperuser
```

### 8. Start Development Server

```bash
python manage.py runserver
```

Visit: http://localhost:8000

## Docker Setup (Alternative)

If using Docker, the setup is much simpler:

```bash
# From project root
docker-compose up -d

# Wait for services to start (10-15 seconds)

# Run migrations
docker-compose exec web python manage.py migrate

# Create superuser
docker-compose exec web python manage.py createsuperuser

# Access API
# http://localhost:8000
# http://localhost:8000/api/docs/
```

## Troubleshooting

### Issue: `ModuleNotFoundError: No module named 'psycopg2'`

**Solution:**
```bash
# Try binary installation
pip install psycopg2-binary --only-binary psycopg2-binary

# Or use pre-built wheel on Windows
pip install psycopg2==2.9.9 --only-binary :all:
```

### Issue: `Failed to build 'Pillow'`

**Solution:** Pillow is optional and not required for core functionality. It's commented out in `requirements.txt`. If you need image processing, install pre-built wheel:

```bash
pip install Pillow --only-binary :all:
```

### Issue: Database connection failed

**Solution:** Make sure PostgreSQL is running:

```bash
# Docker
docker-compose up -d db
docker-compose logs db

# Or check PostgreSQL service on Windows
# Services > PostgreSQL > Status
```

### Issue: `FATAL: role "postgres" does not exist`

**Solution:** Create the default postgres role:

```bash
# Using docker-compose
docker-compose exec db psql -U postgres -c "CREATE DATABASE stayafrica;"
```

## Development Workflow

### Run Tests

```bash
pytest
pytest --cov=apps  # With coverage
```

### Run Linting

```bash
# Using flake8
flake8 apps/

# Using pylint
pylint apps/
```

### Format Code

```bash
# Using black
black .

# Using isort
isort .
```

### Database Migrations

```bash
# Create new migration
python manage.py makemigrations

# Apply migrations
python manage.py migrate

# Show migration status
python manage.py showmigrations
```

## API Documentation

Once running, access API documentation:

- **Swagger UI:** http://localhost:8000/api/docs/
- **ReDoc:** http://localhost:8000/api/redoc/
- **OpenAPI Schema:** http://localhost:8000/api/schema/

## Common Commands

| Command | Purpose |
|---------|---------|
| `python manage.py runserver` | Start dev server |
| `python manage.py migrate` | Apply migrations |
| `python manage.py makemigrations` | Create migrations |
| `python manage.py createsuperuser` | Create admin user |
| `python manage.py collectstatic` | Collect static files |
| `python manage.py shell` | Interactive Python shell |
| `python manage.py test` | Run tests |
| `pytest` | Run tests with pytest |

## Requirements Files

### requirements.txt
- Core dependencies for backend
- PostgreSQL, Redis, Celery
- Django and DRF
- JWT authentication
- No optional packages (safe for Windows)

### requirements-dev.txt
- All of requirements.txt PLUS
- Optional packages like Pillow
- Development tools
- Image processing libraries

## Python Version Support

- ✅ Python 3.11
- ✅ Python 3.12
- ✅ Python 3.13 (Tested with 3.13.9)

## Next Steps

1. Run migrations: `python manage.py migrate`
2. Create superuser: `python manage.py createsuperuser`
3. Start server: `python manage.py runserver`
4. Visit http://localhost:8000/admin/
5. Start developing!

## Resources

- [Django Documentation](https://docs.djangoproject.com/)
- [Django REST Framework](https://www.django-rest-framework.org/)
- [PostgreSQL PostGIS](https://postgis.net/)
- [Celery Documentation](https://docs.celeryproject.io/)

## Getting Help

If you encounter issues:

1. Check this guide for troubleshooting
2. Review error messages carefully
3. Check Django/DRF documentation
4. Open an issue in the repository

---

**Happy coding! 🚀**
