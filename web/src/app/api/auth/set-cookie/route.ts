import { NextResponse } from 'next/server';

export async function POST(req: Request) {
  try {
    const { token, maxAge } = await req.json();
    if (!token || typeof token !== 'string') {
      return NextResponse.json({ ok: false, error: 'Missing token' }, { status: 400 });
    }

    const isSecure = process.env.NODE_ENV === 'production';
    const response = NextResponse.json({ ok: true });

    response.cookies.set({
      name: 'access_token',
      value: token,
      path: '/',
      maxAge: typeof maxAge === 'number' ? maxAge : 60 * 60 * 24,
      sameSite: 'lax',
      secure: isSecure,
    });

    return response;
  } catch (err) {
    return NextResponse.json({ ok: false, error: 'Invalid request' }, { status: 400 });
  }
}
