import { NextRequest, NextResponse } from 'next/server';

const API_BASE = `${process.env.NEXT_PUBLIC_API_BASE_URL}/api/v1`;

export async function POST(
  req: NextRequest,
  { params }: { params: Promise<{ id: string }> }
) {
  const { id } = await params;
  const token = req.headers.get('authorization') || '';
  const res = await fetch(`${API_BASE}/support/bugs/${id}/escalate/`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json', Authorization: token }
  });
  const data = await res.json();
  return NextResponse.json(data, { status: res.status });
}
