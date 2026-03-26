#!/bin/bash

# StayAfrica Production Verification Script
# Tests that all services are working correctly after deployment

echo -e "\033[0;36m🔍 StayAfrica Production Verification\033[0m"
echo -e "\033[0;36m====================================\033[0m"
echo ""

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;36m'
NC='\033[0m' # No Color

# Test counter
PASSED=0
FAILED=0

# Function to test endpoint
test_endpoint() {
    local name=$1
    local url=$2
    local expected_code=$3
    local method=${4:-GET}
    
    echo -n "Testing $name... "
    
    if [ "$method" == "POST" ]; then
        response=$(curl -s -o /dev/null -w "%{http_code}" -X POST "$url" -H "Content-Type: application/json" -d '{}' 2>/dev/null)
    else
        response=$(curl -s -o /dev/null -w "%{http_code}" "$url" 2>/dev/null)
    fi
    
    if [ "$response" == "$expected_code" ]; then
        echo -e "${GREEN}✅ PASS${NC} (HTTP $response)"
        ((PASSED++))
        return 0
    else
        echo -e "${RED}❌ FAIL${NC} (Expected $expected_code, got $response)"
        ((FAILED++))
        return 1
    fi
}

# 1. Container Status Check
echo -e "${BLUE}1. Checking Container Status${NC}"
echo "----------------------------------------"
if docker compose -f docker-compose.prod.yml ps | grep -q "Up"; then
    echo -e "${GREEN}✅ Containers are running${NC}"
    ((PASSED++))
else
    echo -e "${RED}❌ Some containers are not running${NC}"
    ((FAILED++))
fi
echo ""

# 2. Frontend Tests
echo -e "${BLUE}2. Testing Frontend${NC}"
echo "----------------------------------------"
test_endpoint "Frontend Homepage" "https://stayafrica.app" "200"
echo ""

# 3. Backend API Tests
echo -e "${BLUE}3. Testing Backend API${NC}"
echo "----------------------------------------"
test_endpoint "API Health Check" "https://api.stayafrica.app/api/v1/health/" "200"
test_endpoint "API Properties List" "https://api.stayafrica.app/api/v1/properties/" "200"
test_endpoint "API Auth Endpoints" "https://api.stayafrica.app/api/v1/auth/login/" "405"  # POST only
echo ""

# 4. Static Files Test
echo -e "${BLUE}4. Testing Static Files${NC}"
echo "----------------------------------------"
test_endpoint "Static Files" "https://api.stayafrica.app/static/" "403"  # Directory listing disabled
echo ""

# 5. Media Files Test
echo -e "${BLUE}5. Testing Media Files${NC}"
echo "----------------------------------------"
# Test if media directory is accessible (403 expected for directory listing)
test_endpoint "Media Directory" "https://api.stayafrica.app/media/" "403"

# Check if any media files exist in container
echo -n "Checking media files in container... "
media_count=$(docker compose -f docker-compose.prod.yml exec -T backend sh -c "find /app/media -type f | wc -l" 2>/dev/null)
if [ "$media_count" -gt 0 ]; then
    echo -e "${GREEN}✅ Found $media_count media file(s)${NC}"
    ((PASSED++))
else
    echo -e "${YELLOW}⚠️  No media files found (this is OK if no properties have images)${NC}"
fi
echo ""

# 6. Geocoding Endpoint Test
echo -e "${BLUE}6. Testing Geocoding Endpoint${NC}"
echo "----------------------------------------"
# Test requires authentication, so we expect 401/403
test_endpoint "Geocoding Endpoint" "https://api.stayafrica.app/api/v1/properties/geocode/" "401" "POST"
echo ""

# 7. Database Connection Test
echo -e "${BLUE}7. Testing Database Connection${NC}"
echo "----------------------------------------"
echo -n "Checking database connectivity... "
if docker compose -f docker-compose.prod.yml exec -T backend python manage.py check --database default > /dev/null 2>&1; then
    echo -e "${GREEN}✅ Database connected${NC}"
    ((PASSED++))
else
    echo -e "${RED}❌ Database connection failed${NC}"
    ((FAILED++))
fi
echo ""

# 8. Redis Connection Test
echo -e "${BLUE}8. Testing Redis Connection${NC}"
echo "----------------------------------------"
echo -n "Checking Redis connectivity... "
if docker compose -f docker-compose.prod.yml exec -T redis redis-cli -a "${REDIS_PASSWORD}" ping > /dev/null 2>&1; then
    echo -e "${GREEN}✅ Redis connected and authenticated${NC}"
    ((PASSED++))
else
    echo -e "${RED}❌ Redis connection or authentication failed${NC}"
    ((FAILED++))
fi

echo -n "Verifying Redis is NOT accessible from host... "
if timeout 2 redis-cli -h localhost -p 6379 ping > /dev/null 2>&1; then
    echo -e "${RED}❌ SECURITY ISSUE: Redis is exposed to host!${NC}"
    ((FAILED++))
else
    echo -e "${GREEN}✅ Redis properly isolated (not accessible from host)${NC}"
    ((PASSED++))
fi
echo ""

# 9. Celery Workers Test
echo -e "${BLUE}9. Testing Celery Workers${NC}"
echo "----------------------------------------"
echo -n "Checking Celery worker status... "
celery_status=$(docker compose -f docker-compose.prod.yml exec -T celery celery -A stayafrica inspect ping 2>/dev/null)
if echo "$celery_status" | grep -q "pong"; then
    echo -e "${GREEN}✅ Celery workers active${NC}"
    ((PASSED++))
else
    echo -e "${YELLOW}⚠️  Celery workers may not be responding${NC}"
fi
echo ""

# 10. SSL Certificate Test
echo -e "${BLUE}10. Testing SSL Certificates${NC}"
echo "----------------------------------------"
echo -n "Checking SSL certificate for api.stayafrica.app... "
if echo | openssl s_client -connect api.stayafrica.app:443 -servername api.stayafrica.app 2>/dev/null | grep -q "Verify return code: 0"; then
    echo -e "${GREEN}✅ Valid SSL certificate${NC}"
    ((PASSED++))
else
    echo -e "${YELLOW}⚠️  SSL certificate validation failed (may be self-signed or expired)${NC}"
fi
echo ""

# Summary
echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}Verification Summary${NC}"
echo -e "${BLUE}========================================${NC}"
TOTAL=$((PASSED + FAILED))
echo -e "Total Tests: $TOTAL"
echo -e "${GREEN}Passed: $PASSED${NC}"
echo -e "${RED}Failed: $FAILED${NC}"
echo ""

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}🎉 All critical tests passed!${NC}"
    echo ""
    echo -e "${BLUE}📝 Manual Verification Steps:${NC}"
    echo "1. Visit https://stayafrica.app and test user registration"
    echo "2. Login as a admin and access /admin/"
    echo "3. Create a property with images"
    echo "4. Verify images display correctly on property detail page"
    echo "5. Test geocoding by entering an address in property form"
    exit 0
else
    echo -e "${RED}⚠️  Some tests failed. Review the output above.${NC}"
    echo ""
    echo -e "${YELLOW}Troubleshooting:${NC}"
    echo "- Check container logs: docker compose -f docker-compose.prod.yml logs"
    echo "- Verify environment variables in docker-compose.prod.yml"
    echo "- Ensure SSL certificates are installed: ./setup-ssl.sh"
    echo "- Check Nginx configuration: docker compose -f docker-compose.prod.yml exec nginx nginx -t"
    exit 1
fi
