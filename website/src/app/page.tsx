import { AppMain } from '@/components/app';
import Header from '@/components/header';
import { ToastContainer } from 'react-toastify';

import 'react-toastify/dist/ReactToastify.css';

export default function Home() {
  return (
    <main className="flex min-h-screen flex-col items-center justify-start">
      <Header />

      <div className="flex w-full flex-grow">
        <AppMain />
      </div>

      <ToastContainer autoClose={2000} />
    </main>
  );
}
