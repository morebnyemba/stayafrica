#!/usr/bin/env python
"""
Test script to verify analytics URL patterns are correctly registered.
Run this inside the Django environment to check URL resolution.
"""
import os
import sys
import django

# Setup Django
os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'stayafrica.settings')
django.setup()

from django.urls import get_resolver, reverse
from rest_framework.routers import DefaultRouter
from apps.properties.analytics_views import (
    HostAnalyticsViewSet,
    PropertyAnalyticsViewSet,
    PerformanceBenchmarkViewSet
)

print("=" * 60)
print("Analytics URL Pattern Test")
print("=" * 60)

# Test 1: Check if views are importable
print("\n1. Testing view imports...")
try:
    print(f"✓ HostAnalyticsViewSet imported: {HostAnalyticsViewSet}")
    print(f"✓ PropertyAnalyticsViewSet imported: {PropertyAnalyticsViewSet}")
    print(f"✓ PerformanceBenchmarkViewSet imported: {PerformanceBenchmarkViewSet}")
except Exception as e:
    print(f"✗ Error importing views: {e}")
    sys.exit(1)

# Test 2: Check router configuration
print("\n2. Testing router configuration...")
try:
    router = DefaultRouter()
    router.register(r'analytics/host', HostAnalyticsViewSet, basename='host-analytics')
    router.register(r'analytics/properties', PropertyAnalyticsViewSet, basename='property-analytics')
    router.register(r'analytics/benchmarks', PerformanceBenchmarkViewSet, basename='benchmarks')
    
    print(f"✓ Router configured with {len(router.urls)} URL patterns")
    
    print("\nGenerated URL patterns:")
    for pattern in router.urls:
        print(f"  - {pattern.pattern}")
        
except Exception as e:
    print(f"✗ Error configuring router: {e}")
    sys.exit(1)

# Test 3: Check if URLs are resolvable in the main URL conf
print("\n3. Testing URL resolution...")
try:
    resolver = get_resolver()
    
    # Check for analytics patterns
    all_patterns = []
    
    def collect_patterns(pattern_list, prefix=''):
        for pattern in pattern_list:
            pattern_str = str(pattern.pattern)
            full_pattern = prefix + pattern_str
            
            if hasattr(pattern, 'url_patterns'):
                collect_patterns(pattern.url_patterns, full_pattern)
            else:
                all_patterns.append(full_pattern)
    
    collect_patterns(resolver.url_patterns)
    
    analytics_patterns = [p for p in all_patterns if 'analytics' in p]
    
    if analytics_patterns:
        print(f"✓ Found {len(analytics_patterns)} analytics URL patterns:")
        for pattern in analytics_patterns[:20]:  # Show first 20
            print(f"  - {pattern}")
    else:
        print("✗ No analytics URL patterns found!")
        
except Exception as e:
    print(f"✗ Error checking URL resolution: {e}")
    import traceback
    traceback.print_exc()

# Test 4: Try to reverse URLs
print("\n4. Testing URL reversing...")
test_urls = [
    ('properties:host-analytics-dashboard', {}, 'dashboard action'),
    ('properties:host-analytics-revenue-chart', {}, 'revenue_chart action'),
    ('properties:host-analytics-occupancy-trend', {}, 'occupancy_trend action'),
    ('properties:property-analytics-list', {}, 'property analytics list'),
    ('properties:benchmarks-list', {}, 'benchmarks list'),
]

for url_name, kwargs, description in test_urls:
    try:
        url = reverse(url_name, kwargs=kwargs)
        print(f"✓ {description}: {url}")
    except Exception as e:
        print(f"✗ {description}: {e}")

print("\n" + "=" * 60)
print("Test Complete")
print("=" * 60)
