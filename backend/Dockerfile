FROM python:3.11-slim

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PIP_NO_CACHE_DIR=1

WORKDIR /app

# Install system dependencies for GeoDjango + Postgres + Pillow
RUN apt-get update && apt-get install -y \
    build-essential \
    libpq-dev \
    gdal-bin \
    libgdal-dev \
    libproj-dev \
    libjpeg-dev \
    zlib1g-dev \
    libfreetype6-dev \
    liblcms2-dev \
    libopenjp2-7-dev \
    libtiff-dev \
    postgresql-client \
    && rm -rf /var/lib/apt/lists/*

# Install Python dependencies
COPY requirements.txt .
RUN pip install --upgrade pip \
    && pip install --no-cache-dir -r requirements.txt

# Copy project
COPY . .

# Collect static assets at build time (safe for local/static storage)
RUN python manage.py collectstatic --noinput

EXPOSE 8000

# Use Daphne ASGI server directly
# Migrations are handled by docker-compose depends_on and a separate command
CMD ["daphne", "-b", "0.0.0.0", "-p", "8000", "stayafrica.asgi:application"]
