import http from 'k6/http';
import { check, sleep, group } from 'k6';
import { Rate, Trend } from 'k6/metrics';

const BASE_URL = __ENV.API_URL || 'https://api.stayafrica.app';
const API = `${BASE_URL}/api/v1`;

// Custom metrics
const errorRate = new Rate('errors');
const bookingDuration = new Trend('booking_flow_duration');

export const options = {
  stages: [
    { duration: '2m', target: 20 },   // Ramp up
    { duration: '5m', target: 50 },   // Sustain
    { duration: '3m', target: 100 },  // Peak load
    { duration: '2m', target: 50 },   // Scale down
    { duration: '1m', target: 0 },    // Ramp down
  ],
  thresholds: {
    http_req_duration: ['p(95)<2000', 'p(99)<5000'],
    errors: ['rate<0.05'],
    http_req_failed: ['rate<0.05'],
  },
};

const TEST_USER = {
  email: `loadtest+${Date.now()}@stayafrica.app`,
  password: 'LoadTest123!',
};

function getAuthToken() {
  const loginRes = http.post(`${API}/users/login/`, JSON.stringify({
    email: TEST_USER.email,
    password: TEST_USER.password,
  }), { headers: { 'Content-Type': 'application/json' } });

  if (loginRes.status === 200) {
    return JSON.parse(loginRes.body).access;
  }
  return null;
}

export default function () {
  group('Health Check', () => {
    const res = http.get(`${BASE_URL}/api/health/`);
    check(res, { 'health check OK': (r) => r.status === 200 });
    errorRate.add(res.status !== 200);
  });

  group('Property Search', () => {
    const res = http.get(`${API}/properties/?page=1&page_size=20`);
    check(res, {
      'properties loaded': (r) => r.status === 200,
      'has results': (r) => JSON.parse(r.body).results?.length > 0,
    });
    errorRate.add(res.status !== 200);
  });

  group('Property Detail', () => {
    const listRes = http.get(`${API}/properties/?page=1&page_size=1`);
    if (listRes.status === 200) {
      const properties = JSON.parse(listRes.body).results;
      if (properties?.length > 0) {
        const res = http.get(`${API}/properties/${properties[0].id}/`);
        check(res, { 'property detail loaded': (r) => r.status === 200 });
        errorRate.add(res.status !== 200);
      }
    }
  });

  group('Search with Filters', () => {
    const res = http.get(`${API}/properties/?country=ZW&min_price=10&max_price=500&guests=2`);
    check(res, { 'filtered search OK': (r) => r.status === 200 });
    errorRate.add(res.status !== 200);
  });

  group('System Config', () => {
    const res = http.get(`${API}/admin/config/fees/`);
    check(res, { 'config loaded': (r) => r.status === 200 });
    errorRate.add(res.status !== 200);
  });

  sleep(Math.random() * 3 + 1);
}

export function handleSummary(data) {
  return {
    'stdout': textSummary(data, { indent: ' ', enableColors: true }),
    'load-test-results.json': JSON.stringify(data, null, 2),
  };
}

function textSummary(data, opts) {
  const metrics = data.metrics;
  return `
=== StayAfrica Load Test Results ===
Duration: ${data.state?.testRunDurationMs}ms
VUs Max: ${metrics.vus_max?.values?.max || 'N/A'}

HTTP Requests:
  Total:    ${metrics.http_reqs?.values?.count || 0}
  Failed:   ${metrics.http_req_failed?.values?.passes || 0}
  Duration: p50=${metrics.http_req_duration?.values?.med?.toFixed(0)}ms p95=${metrics.http_req_duration?.values?.['p(95)']?.toFixed(0)}ms p99=${metrics.http_req_duration?.values?.['p(99)']?.toFixed(0)}ms

Custom:
  Error Rate: ${((metrics.errors?.values?.rate || 0) * 100).toFixed(2)}%
=====================================
`;
}
