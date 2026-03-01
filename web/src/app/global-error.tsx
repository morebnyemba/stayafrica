'use client';

export default function GlobalError({
  error,
  reset,
}: {
  error: Error & { digest?: string };
  reset: () => void;
}) {
  return (
    <html>
      <body style={{ margin: 0, fontFamily: 'system-ui, sans-serif', backgroundColor: '#f5f0eb' }}>
        <div style={{ minHeight: '100vh', display: 'flex', alignItems: 'center', justifyContent: 'center', padding: '1rem' }}>
          <div style={{ textAlign: 'center', maxWidth: '32rem' }}>
            <h1 style={{ fontSize: '2rem', fontWeight: 'bold', color: '#1a1a2e', marginBottom: '1rem' }}>
              Critical Error
            </h1>
            <p style={{ color: '#666', marginBottom: '2rem' }}>
              Something went seriously wrong. We&apos;ve been notified and are working on a fix.
            </p>
            {error.digest && (
              <p style={{ fontSize: '0.875rem', color: '#999', marginBottom: '1.5rem', fontFamily: 'monospace' }}>
                Error ID: {error.digest}
              </p>
            )}
            <button
              onClick={reset}
              style={{
                padding: '0.75rem 1.5rem',
                backgroundColor: '#2d6a4f',
                color: 'white',
                border: 'none',
                borderRadius: '9999px',
                fontSize: '1rem',
                fontWeight: 500,
                cursor: 'pointer',
              }}
            >
              Try Again
            </button>
          </div>
        </div>
      </body>
    </html>
  );
}
