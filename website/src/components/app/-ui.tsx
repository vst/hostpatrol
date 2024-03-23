import { Card, CardBody, CardHeader } from '@nextui-org/card';
import { Listbox, ListboxItem } from '@nextui-org/listbox';
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

export function KVBox({
  title,
  kvs,
}: {
  title: string;
  kvs: { key: string; value: React.ReactNode | string | number | null | undefined }[];
}) {
  return (
    <Card radius="sm" shadow="sm">
      <CardHeader className="text-lg font-bold">{title}</CardHeader>

      <CardBody>
        <Listbox items={kvs}>
          {({ key, value }) => (
            <ListboxItem key={key} endContent={value}>
              {key}
            </ListboxItem>
          )}
        </Listbox>
      </CardBody>
    </Card>
  );
}
