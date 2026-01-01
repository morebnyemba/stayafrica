#!/bin/bash

# Create necessary directories for SSL certificates
mkdir -p nginx/certbot/conf
mkdir -p nginx/certbot/www

echo "Directories created for SSL setup"
echo ""
echo "To obtain SSL certificates, run:"
echo "  ./setup-ssl.sh"
