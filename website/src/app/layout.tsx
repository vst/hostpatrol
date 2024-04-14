import Header from '@/components/header';
import type { Metadata } from 'next';
import { JetBrains_Mono } from 'next/font/google';
import { ToastContainer } from 'react-toastify';
import 'react-toastify/dist/ReactToastify.css';
import './globals.css';
import { Providers } from './providers';

const font = JetBrains_Mono({ subsets: ['latin'] });

export const metadata: Metadata = {
  title: 'Host Patrol',
  description: "Lazy Hacker's Host Patrol",
};

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  return (
    <html lang="en">
      <body className={`bg-gray-50 ${font.className}`}>
        <Providers>
          <main className="flex min-h-screen flex-col items-center justify-start">
            <Header />
            <div className="flex w-full flex-grow">{children}</div>
            <ToastContainer autoClose={2000} />
          </main>
        </Providers>
      </body>
    </html>
  );
}
