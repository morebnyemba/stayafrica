#!/bin/bash

# StayAfrica Deployment Setup Script
# Run this on your Ubuntu/Debian server with: sudo bash deploy-setup.sh

set -e

echo "===== StayAfrica Deployment Setup ====="
echo "This script will install Docker, Portainer, snapd, certbot, and dependencies"
echo ""

# Check if running as root
if [[ $EUID -ne 0 ]]; then
   echo "This script must be run as root (use: sudo bash deploy-setup.sh)"
   exit 1
fi

# Update system
echo "[1/6] Updating system packages..."
apt-get update
apt-get upgrade -y

# Install Docker
echo "[2/6] Installing Docker..."
apt-get install -y \
    apt-transport-https \
    ca-certificates \
    curl \
    gnupg \
    lsb-release

# Add Docker GPG key
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg

# Add Docker repository
echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu \
  $(lsb_release -cs) stable" | tee /etc/apt/sources.list.d/docker.list > /dev/null

# Install Docker packages
apt-get update
apt-get install -y docker-ce docker-ce-cli containerd.io docker-compose-plugin

# Enable Docker service
systemctl enable docker
systemctl start docker

# Add current user to docker group (optional, for non-root usage)
if [ "$SUDO_USER" ]; then
    usermod -aG docker "$SUDO_USER"
    echo "Added $SUDO_USER to docker group"
fi

# Install snapd
echo "[3/6] Installing snapd..."
apt-get install -y snapd

# Ensure snapd is running
systemctl enable snapd
systemctl start snapd

# Wait for snapd to initialize
sleep 10

# Install certbot via snap
echo "[4/6] Installing certbot via snap..."
snap install --classic certbot

# Create symlink for certbot
ln -sf /snap/bin/certbot /usr/bin/certbot || true

# Install additional dependencies
echo "[5/6] Installing additional dependencies..."
apt-get install -y \
    curl \
    wget \
    git \
    nano \
    htop \
    net-tools \
    ufw \
    openssl

# Setup firewall (UFW)
echo "[6/6] Configuring firewall..."
ufw --force enable
ufw default deny incoming
ufw default allow outgoing
ufw allow 22/tcp    # SSH
ufw allow 80/tcp    # HTTP
ufw allow 443/tcp   # HTTPS
ufw allow 9000/tcp  # Portainer
ufw allow 9443/tcp  # Portainer HTTPS

echo ""
echo "✓ System packages installed"
echo ""
echo "===== Docker Setup ====="

# Check Docker version
echo "Docker version:"
docker --version
echo ""
echo "Docker Compose version:"
docker compose version
echo ""

# Install Portainer
echo "[Optional] Installing Portainer (Docker management UI)..."
read -p "Install Portainer? (y/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    docker volume create portainer_data
    docker run -d \
        --name portainer \
        --restart always \
        -p 8000:8000 \
        -p 9000:9000 \
        -p 9443:9443 \
        -v /var/run/docker.sock:/var/run/docker.sock \
        -v portainer_data:/data \
        portainer/portainer-ce:latest
    echo "✓ Portainer installed"
    echo "  Access at: https://your-server-ip:9443"
    echo ""
fi

# Create certificate directories
echo "===== Certificate Setup ====="
mkdir -p /var/www/certbot
chmod -R 755 /var/www/certbot

echo ""
echo "✓ Certificate directory created at /var/www/certbot"
echo ""

# Prompt for certificate generation
read -p "Generate SSL certificates now? (y/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    read -p "Enter your email for Let's Encrypt notifications: " EMAIL
    
    echo "Generating certificate for stayafrica.app..."
    certbot certonly --webroot \
        -w /var/www/certbot \
        -d stayafrica.app \
        -d www.stayafrica.app \
        --email "$EMAIL" \
        --agree-tos \
        --non-interactive
    
    echo "Generating certificate for api.stayafrica.app..."
    certbot certonly --webroot \
        -w /var/www/certbot \
        -d api.stayafrica.app \
        --email "$EMAIL" \
        --agree-tos \
        --non-interactive
    
    echo ""
    echo "✓ Certificates generated successfully"
    echo ""
fi

# Setup auto-renewal
echo "Setting up automatic certificate renewal..."
systemctl enable snap.certbot.renew.timer
echo "✓ Certbot auto-renewal enabled"
echo ""

# Summary
echo "===== Setup Complete ====="
echo ""
echo "Next steps:"
echo "1. Push your docker-compose.prod.yml to the server"
echo "2. Copy your .env and .env.prod files to the server"
echo "3. Run: docker compose -f docker-compose.prod.yml up -d"
echo ""
echo "Certificates location: /etc/letsencrypt/live/"
echo "Nginx config should point to these certificate paths"
echo ""
echo "Portainer: https://your-server-ip:9443"
echo "Application: https://stayafrica.app"
echo "API: https://api.stayafrica.app"
echo ""
