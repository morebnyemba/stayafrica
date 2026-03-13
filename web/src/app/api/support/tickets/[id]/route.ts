import { NextRequest, NextResponse } from 'next/server';

const API_BASE = `${process.env.NEXT_PUBLIC_API_BASE_URL}/api/v1`;

export async function GET(
  req: NextRequest,
  { params }: { params: { id: string } }
) {
  const token = req.headers.get('authorization') || '';
  const res = await fetch(`${API_BASE}/support/tickets/${params.id}/`, {
    headers: { Authorization: token }
  });
  const data = await res.json();
  return NextResponse.json(data, { status: res.status });
}
