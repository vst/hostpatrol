import Link from 'next/link';

export default function Page() {
  return (
    <div className="flex w-full flex-col items-center justify-center">
      <div>
        <p>Landing page is coming soon...</p>
        <p>
          Check{' '}
          <Link href="/report" className="font-bold text-blue-500">
            report
          </Link>{' '}
          page.
        </p>
      </div>
    </div>
  );
}
