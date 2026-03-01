import http from 'k6/http';
import { check, sleep } from 'k6';
import { Rate } from 'k6/metrics';

const BASE_URL = __ENV.API_URL || 'https://api.stayafrica.app';
const API = `${BASE_URL}/api/v1`;

const errorRate = new Rate('errors');

export const options = {
  stages: [
    { duration: '1m', target: 50 },
    { duration: '2m', target: 200 },
    { duration: '3m', target: 500 },
    { duration: '2m', target: 1000 },
    { duration: '1m', target: 0 },
  ],
  thresholds: {
    http_req_duration: ['p(95)<5000'],
    errors: ['rate<0.10'],
  },
};

export default function () {
  const res = http.get(`${API}/properties/?page=1&page_size=10`);
  check(res, { 'status 200': (r) => r.status === 200 });
  errorRate.add(res.status !== 200);
  sleep(0.5);
}
