import { NextRequest, NextResponse } from 'next/server';

const API_BASE = `${process.env.NEXT_PUBLIC_API_BASE_URL}/api/v1`;

export async function POST(
  req: NextRequest,
  { params }: { params: Promise<{ id: string }> }
) {
  const { id } = await params;
  const token = req.headers.get('authorization') || '';
  const body = await req.json().catch(() => ({}));
  const res = await fetch(`${API_BASE}/support/tickets/${id}/change_status/`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json', Authorization: token },
    body: JSON.stringify(body)
  });
  const data = await res.json();
  return NextResponse.json(data, { status: res.status });
}
