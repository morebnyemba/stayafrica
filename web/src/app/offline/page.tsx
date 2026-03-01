export default function OfflinePage() {
  return (
    <div className="flex min-h-screen flex-col items-center justify-center bg-sand-100 px-4 text-center">
      <div className="mx-auto max-w-md">
        <div className="mb-6 text-6xl">📡</div>
        <h1 className="mb-4 text-3xl font-bold text-primary-900">
          You&apos;re Offline
        </h1>
        <p className="mb-8 text-lg text-gray-600">
          It looks like you&apos;ve lost your internet connection. Please check
          your connection and try again.
        </p>
        <button
          onClick={() => window.location.reload()}
          className="rounded-lg bg-primary-600 px-6 py-3 text-white transition hover:bg-primary-700"
        >
          Try Again
        </button>
      </div>
    </div>
  );
}
