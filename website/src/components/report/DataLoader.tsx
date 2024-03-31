import { LhpPatrolReport, parseData, saveData } from '@/lib/data';
import { Card, CardBody, CardFooter, CardHeader } from '@nextui-org/card';
import { Divider } from '@nextui-org/divider';
import { ChangeEvent, useState } from 'react';
import { Centered } from '../helpers';

export function DataLoader({ onLoadData }: { onLoadData: (x: LhpPatrolReport) => void }) {
  const [error, setError] = useState<string>();

  const changeHandler = (e: ChangeEvent<HTMLInputElement | HTMLTextAreaElement>) => {
    setError(undefined);

    const files = (e.target as HTMLInputElement).files;

    if (files == null || files.length === 0) {
      return;
    }

    const fr = new FileReader();
    fr.onloadend = () =>
      parseData(fr.result as string).caseOf({
        Left: setError,
        Right(data) {
          saveData(data);
          onLoadData(data);
        },
      });
    fr.readAsText(files[0]);
  };

  return (
    <Centered>
      <Card radius="sm" shadow="sm" fullWidth={true} classNames={{ base: 'max-w-xl' }}>
        <CardHeader className="text-lg font-bold">Load Data</CardHeader>

        <Divider />

        <CardBody>
          <input type="file" id="image" accept=".JSON" onChange={changeHandler} />
        </CardBody>

        {error && (
          <CardFooter className="bg-red-500 text-white">
            <p>{error}</p>
          </CardFooter>
        )}
      </Card>
    </Centered>
  );
}
