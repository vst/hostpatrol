import { Spinner } from '@nextui-org/react';

export function Centered({ children }: { children: React.ReactNode }) {
  return <div className="flex flex-grow items-center justify-center">{children}</div>;
}

export function BigSpinner({ label }: { label?: string }) {
  return (
    <Centered>
      <Spinner label={label} size="lg" color="default" />
    </Centered>
  );
}
