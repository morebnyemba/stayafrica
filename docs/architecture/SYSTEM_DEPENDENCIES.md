# System Dependencies

This document lists the system-level dependencies required to run the StayAfrica application locally.

## Overview

The StayAfrica backend uses Django with GeoDjango for spatial/geographic features. This requires additional system libraries beyond Python packages.

## Required System Libraries

### For Linux (Ubuntu/Debian)

```bash
# Update package list
sudo apt-get update

# Install GDAL (Geographic Data Abstraction Library)
sudo apt-get install -y gdal-bin libgdal-dev libproj-dev

# Install SpatiaLite (for development with SQLite + spatial support)
sudo apt-get install -y libsqlite3-mod-spatialite

# Install other dependencies
sudo apt-get install -y build-essential libpq-dev
```

### For macOS

```bash
# Using Homebrew
brew install gdal
brew install spatialite-tools
brew install postgresql
```

### For Windows

1. Download and install OSGeo4W from https://trac.osgeo.org/osgeo4w/
2. Or use Windows Subsystem for Linux (WSL) and follow Linux instructions

## Python Dependencies

After installing system dependencies, install Python packages:

```bash
cd backend
pip install -r requirements.txt
```

## Verification

To verify that all dependencies are correctly installed:

```bash
cd backend
DEBUG=True python manage.py check
```

You should see:
```
System check identified some issues:

WARNINGS:
?: (urls.W005) URL namespace 'admin' isn't unique. You may not be able to reverse all URLs in this namespace

System check identified 1 issue (0 silenced).
```

## Common Issues

### "Could not find the GDAL library"

**Solution:** Install GDAL system libraries as shown above.

```bash
sudo apt-get install -y gdal-bin libgdal-dev
```

### "Module doesn't declare an explicit app_label"

**Solution:** Ensure all apps referenced in `urls.py` are listed in `INSTALLED_APPS` in `settings.py`.

### Database Connection Errors in Development

If you see PostgreSQL connection errors while in development mode, make sure you're setting `DEBUG=True`:

```bash
DEBUG=True python manage.py <command>
```

Or create a `.env` file in the backend directory:

```
DEBUG=True
```

## Production Deployment

For production deployment using Docker, all system dependencies are already included in the `Dockerfile`. No additional setup is needed.

## Database Backends

### Development (DEBUG=True)
- Uses SQLite with SpatiaLite extension
- Location: `backend/db.sqlite3`
- Requires: libsqlite3-mod-spatialite

### Production (DEBUG=False)
- Uses PostgreSQL with PostGIS extension
- Requires: PostgreSQL 12+ with PostGIS extension
- Connection configured via environment variables

## Additional Resources

- [GeoDjango Installation](https://docs.djangoproject.com/en/5.0/ref/contrib/gis/install/)
- [GDAL Documentation](https://gdal.org/)
- [SpatiaLite Documentation](https://www.gaia-gis.it/fossil/libspatialite/index)
