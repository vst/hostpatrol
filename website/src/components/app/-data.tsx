import { Card, CardBody, CardFooter, CardHeader } from '@nextui-org/card';
import { Divider } from '@nextui-org/divider';
import { Either, Left, Right } from 'purify-ts/Either';
import { Just, Maybe, Nothing } from 'purify-ts/Maybe';
import { ChangeEvent, useState } from 'react';
import { Centered } from './-ui';

export interface LhpData {
  host: {
    name: string;
    tags?: string[];
    url?: string;
    [k: string]: unknown;
  };
  hardware: {
    cpuCount: number;
    diskRoot: number;
    ramTotal: number;
    [k: string]: unknown;
  };
  kernel: {
    machine: string;
    name: string;
    node: string;
    os: string;
    release: string;
    version: string;
    [k: string]: unknown;
  };
  distribution: {
    description: string;
    codename: null | string;
    id: string;
    name: string;
    release: string;
    version: string;
    [k: string]: unknown;
  };
  cloud: {
    hostAvailabilityZone: null | string;
    hostLocalAddress: null | string;
    hostLocalHostname: null | string;
    hostRegion: null | string;
    hostRemoteAddress: null | string;
    hostRemoteHostname: null | string;
    hostReservedAddress: null | string;
    hostType: null | string;
    id: null | string;
    name: string;
    [k: string]: unknown;
  };
  dockerContainers:
    | null
    | {
        created: {
          [k: string]: unknown;
        };
        id: string;
        image: string;
        name: string;
        running: boolean;
        [k: string]: unknown;
      }[];
  [k: string]: unknown;
}

const _LOCAL_STORAGE_KEY_DATA = 'LHP_DATA';

export function loadData(): Either<string, Maybe<LhpData[]>> {
  const data = localStorage.getItem(_LOCAL_STORAGE_KEY_DATA);

  if (data === null) {
    return Right(Nothing);
  }

  return parseData(data).map(Just);
}

export function saveData(x: LhpData[]): void {
  localStorage.setItem(_LOCAL_STORAGE_KEY_DATA, JSON.stringify(x));
}

export function deleteData(): void {
  localStorage.removeItem(_LOCAL_STORAGE_KEY_DATA);
}

export function parseData(raw: string): Either<string, LhpData[]> {
  try {
    return Right(JSON.parse(raw) as LhpData[]); // TODO: Parse (or validate)
  } catch (err) {
    return Left(`Data can not be parsed into JSON. ${err}`);
  }
}

export function DataLoader({ onLoadData }: { onLoadData: (x: LhpData[]) => void }) {
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
