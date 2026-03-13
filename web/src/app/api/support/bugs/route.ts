import { NextRequest, NextResponse } from 'next/server';

const API_BASE = `${process.env.NEXT_PUBLIC_API_BASE_URL}/api/v1`;

export async function GET(req: NextRequest) {
  const token = req.headers.get('authorization') || '';
  const search = req.nextUrl.search;
  const res = await fetch(`${API_BASE}/support/bugs/${search}`, {
    headers: { Authorization: token }
  });
  const data = await res.json();
  return NextResponse.json(data, { status: res.status });
}

export async function POST(req: NextRequest) {
  const token = req.headers.get('authorization') || '';
  const body = await req.json();
  const res = await fetch(`${API_BASE}/support/bugs/`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json', Authorization: token },
    body: JSON.stringify(body)
  });
  const data = await res.json();
  return NextResponse.json(data, { status: res.status });
}
