#!/bin/bash

# Redis Security Verification Script for StayAfrica
# This script verifies that Redis is properly secured

echo -e "\033[0;36m🔒 Redis Security Verification\033[0m"
echo -e "\033[0;36m==============================\033[0m"
echo ""

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;36m'
NC='\033[0m' # No Color

PASSED=0
FAILED=0

# Check if .env file exists and has REDIS_PASSWORD
echo -e "${BLUE}1. Checking Environment Configuration${NC}"
echo "----------------------------------------"
if [ -f .env ]; then
    echo -e "${GREEN}✅ .env file exists${NC}"
    ((PASSED++))
    
    if grep -q "REDIS_PASSWORD=" .env; then
        echo -e "${GREEN}✅ REDIS_PASSWORD is set in .env${NC}"
        ((PASSED++))
        
        # Check if password is not the default weak one
        if grep -q "REDIS_PASSWORD=ChangeThisToAStrongRedisPassword123" .env; then
            echo -e "${YELLOW}⚠️  WARNING: Using example password from .env.example${NC}"
            echo -e "${YELLOW}   Please change REDIS_PASSWORD to a strong unique password!${NC}"
        elif grep -q "REDIS_PASSWORD=devpassword123" .env; then
            echo -e "${YELLOW}⚠️  WARNING: Using development default password${NC}"
            echo -e "${YELLOW}   For production, use a strong unique password!${NC}"
        else
            echo -e "${GREEN}✅ Custom Redis password is configured${NC}"
            ((PASSED++))
        fi
    else
        echo -e "${RED}❌ REDIS_PASSWORD not found in .env${NC}"
        ((FAILED++))
    fi
else
    echo -e "${RED}❌ .env file not found${NC}"
    echo -e "${YELLOW}   Copy .env.example to .env and configure Redis password${NC}"
    ((FAILED++))
fi
echo ""

# Check docker-compose.yml configuration
echo -e "${BLUE}2. Checking Docker Compose Configuration${NC}"
echo "----------------------------------------"

# Development config
if [ -f docker-compose.yml ]; then
    echo -n "Checking docker-compose.yml (development)... "
    if grep -A10 "redis:" docker-compose.yml | grep -q "ports:.*6379"; then
        echo -e "${RED}❌ Redis port exposed in docker-compose.yml${NC}"
        ((FAILED++))
    else
        echo -e "${GREEN}✅ Redis port not exposed${NC}"
        ((PASSED++))
    fi
    
    if grep -A10 "redis:" docker-compose.yml | grep -q "requirepass"; then
        echo -e "${GREEN}✅ Redis password authentication enabled${NC}"
        ((PASSED++))
    else
        echo -e "${RED}❌ Redis password not configured${NC}"
        ((FAILED++))
    fi
fi

# Production config
if [ -f docker-compose.prod.yml ]; then
    echo -n "Checking docker-compose.prod.yml (production)... "
    if grep -A10 "redis:" docker-compose.prod.yml | grep -q "ports:.*6379"; then
        echo -e "${RED}❌ Redis port exposed in docker-compose.prod.yml${NC}"
        ((FAILED++))
    else
        echo -e "${GREEN}✅ Redis port not exposed${NC}"
        ((PASSED++))
    fi
    
    if grep -A10 "redis:" docker-compose.prod.yml | grep -q "requirepass"; then
        echo -e "${GREEN}✅ Redis password authentication enabled${NC}"
        ((PASSED++))
    else
        echo -e "${RED}❌ Redis password not configured${NC}"
        ((FAILED++))
    fi
fi
echo ""

# Check if Redis is running
echo -e "${BLUE}3. Checking Redis Container Status${NC}"
echo "----------------------------------------"
if docker ps | grep -q stayafrica_redis; then
    echo -e "${GREEN}✅ Redis container is running${NC}"
    ((PASSED++))
    
    # Test 1: Verify Redis requires authentication
    echo -n "Testing authentication requirement... "
    if docker exec stayafrica_redis redis-cli ping 2>&1 | grep -q "NOAUTH"; then
        echo -e "${GREEN}✅ Redis requires authentication${NC}"
        ((PASSED++))
    elif docker exec stayafrica_redis redis-cli ping 2>&1 | grep -q "PONG"; then
        echo -e "${RED}❌ Redis accepts connections without password!${NC}"
        ((FAILED++))
    else
        echo -e "${YELLOW}⚠️  Unable to verify authentication requirement${NC}"
    fi
    
    # Test 2: Check if Redis is accessible from host
    echo -n "Verifying Redis network isolation... "
    if timeout 2 redis-cli -h localhost -p 6379 ping > /dev/null 2>&1; then
        echo -e "${RED}❌ CRITICAL: Redis is accessible from host!${NC}"
        echo -e "${RED}   This is a security vulnerability!${NC}"
        ((FAILED++))
    elif timeout 2 nc -zv localhost 6379 > /dev/null 2>&1; then
        echo -e "${RED}❌ CRITICAL: Redis port is open on host!${NC}"
        ((FAILED++))
    else
        echo -e "${GREEN}✅ Redis is not accessible from host (properly isolated)${NC}"
        ((PASSED++))
    fi
    
    # Test 3: Verify password authentication works from backend
    echo -n "Testing authenticated access from backend... "
    if docker exec stayafrica_backend sh -c 'echo "ping" | redis-cli -h redis -a "$REDIS_PASSWORD"' 2>/dev/null | grep -q "PONG"; then
        echo -e "${GREEN}✅ Backend can connect to Redis with password${NC}"
        ((PASSED++))
    else
        echo -e "${RED}❌ Backend cannot connect to Redis${NC}"
        ((FAILED++))
    fi
    
    # Test 4: Check Redis configuration
    echo -n "Checking Redis configuration security... "
    # Check if protected mode is enabled
    if docker exec stayafrica_redis redis-cli -a "${REDIS_PASSWORD}" CONFIG GET protected-mode 2>/dev/null | grep -q "yes"; then
        echo -e "${GREEN}✅ Protected mode enabled${NC}"
        ((PASSED++))
    else
        echo -e "${YELLOW}⚠️  Protected mode not explicitly enabled${NC}"
    fi
    
else
    echo -e "${YELLOW}⚠️  Redis container is not running${NC}"
    echo "   Start containers to run live security tests"
fi
echo ""

# Check service connection strings
echo -e "${BLUE}4. Checking Service Connection Strings${NC}"
echo "----------------------------------------"

# Check backend service
if [ -f docker-compose.yml ]; then
    echo -n "Backend Redis URL... "
    if grep -A20 "backend:" docker-compose.yml | grep "REDIS_URL" | grep -q "REDIS_PASSWORD"; then
        echo -e "${GREEN}✅ Contains password${NC}"
        ((PASSED++))
    else
        echo -e "${RED}❌ Missing password in connection string${NC}"
        ((FAILED++))
    fi
    
    echo -n "Celery Redis URL... "
    if grep -A20 "celery:" docker-compose.yml | grep "CELERY_BROKER_URL" | grep -q "REDIS_PASSWORD"; then
        echo -e "${GREEN}✅ Contains password${NC}"
        ((PASSED++))
    else
        echo -e "${RED}❌ Missing password in connection string${NC}"
        ((FAILED++))
    fi
fi
echo ""

# Summary
echo -e "${BLUE}==============================${NC}"
echo -e "${BLUE}Security Verification Summary${NC}"
echo -e "${BLUE}==============================${NC}"
TOTAL=$((PASSED + FAILED))
echo -e "Total Checks: $TOTAL"
echo -e "${GREEN}Passed: $PASSED${NC}"
echo -e "${RED}Failed: $FAILED${NC}"
echo ""

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}🎉 Redis is properly secured!${NC}"
    echo ""
    echo -e "${BLUE}Security measures verified:${NC}"
    echo "✅ Password authentication enabled"
    echo "✅ No public port exposure"
    echo "✅ Network isolation (Docker internal only)"
    echo "✅ Service connections use authentication"
    echo ""
    exit 0
else
    echo -e "${RED}⚠️  Security issues detected!${NC}"
    echo ""
    echo -e "${YELLOW}Required Actions:${NC}"
    
    if ! grep -q "REDIS_PASSWORD=" .env 2>/dev/null; then
        echo "1. Set REDIS_PASSWORD in .env file:"
        echo "   openssl rand -base64 32 | sed 's/=//g'"
    fi
    
    if grep -q "ports:" docker-compose.yml 2>/dev/null | grep -A1 "redis:" | grep -q "6379"; then
        echo "2. Remove 'ports:' section from redis service in docker-compose.yml"
    fi
    
    echo "3. Update all Redis connection strings to include password:"
    echo "   redis://:\${REDIS_PASSWORD}@redis:6379/0"
    
    echo "4. Restart services:"
    echo "   docker-compose down && docker-compose up -d"
    echo ""
    echo "For detailed instructions, see: REDIS_SECURITY.md"
    exit 1
fi
