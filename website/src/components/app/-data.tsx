import { Card, CardBody, CardFooter, CardHeader } from '@nextui-org/card';
import { Divider } from '@nextui-org/divider';
import Ajv from 'ajv';
import type { JSONSchema } from 'json-schema-to-ts';
import { FromSchema } from 'json-schema-to-ts';
import { Either, Left, Right } from 'purify-ts/Either';
import { Just, Maybe, Nothing } from 'purify-ts/Maybe';
import { ChangeEvent, useState } from 'react';
import { Centered } from './-ui';

export const LHP_PATROL_REPORT_SCHEMA = {
  $comment: 'Host Patrol Report\nReport',
  properties: {
    cloud: {
      $comment: 'Cloud information.\nCloud Information\nCloud',
      properties: {
        hostAvailabilityZone: { $comment: 'Host availability zone.', anyOf: [{ type: 'null' }, { type: 'string' }] },
        hostLocalAddress: { $comment: 'Local address of the host.', anyOf: [{ type: 'null' }, { type: 'string' }] },
        hostLocalHostname: { $comment: 'Local hostname of the host.', anyOf: [{ type: 'null' }, { type: 'string' }] },
        hostRegion: { $comment: 'Host region.', anyOf: [{ type: 'null' }, { type: 'string' }] },
        hostRemoteAddress: { $comment: 'Remote address of the host.', anyOf: [{ type: 'null' }, { type: 'string' }] },
        hostRemoteHostname: { $comment: 'Remote hostname of the host.', anyOf: [{ type: 'null' }, { type: 'string' }] },
        hostReservedAddress: {
          $comment: 'Reserved address of the host.',
          anyOf: [{ type: 'null' }, { type: 'string' }],
        },
        hostType: { $comment: 'Host type.', anyOf: [{ type: 'null' }, { type: 'string' }] },
        id: { $comment: 'Host identifier.', anyOf: [{ type: 'null' }, { type: 'string' }] },
        name: { $comment: 'Cloud name.', type: 'string' },
      },
      required: [
        'hostReservedAddress',
        'hostRemoteAddress',
        'hostRemoteHostname',
        'hostLocalAddress',
        'hostLocalHostname',
        'hostAvailabilityZone',
        'hostRegion',
        'hostType',
        'id',
        'name',
      ],
      type: 'object',
    },
    distribution: {
      $comment: 'Distribution information.\nDistribution Information\nDistribution',
      properties: {
        codename: {
          $comment: "Distribution codename (cat /etc/os-release | grep 'VERSION_CODENAME=').",
          anyOf: [{ type: 'null' }, { type: 'string' }],
        },
        description: {
          $comment: "Distribution description (cat /etc/os-release | grep 'PRETTY_NAME=').",
          type: 'string',
        },
        id: { $comment: "Distribution ID (cat /etc/os-release | grep 'ID=').", type: 'string' },
        name: { $comment: "Distribution name (cat /etc/os-release | grep 'NAME=')).", type: 'string' },
        release: { $comment: "Distribution release (cat /etc/os-release | grep 'VERSION_ID=').", type: 'string' },
        version: { $comment: "Distribution version (cat /etc/os-release | grep 'VERSION=').", type: 'string' },
      },
      required: ['description', 'codename', 'release', 'version', 'name', 'id'],
      type: 'object',
    },
    dockerContainers: {
      $comment: 'List of Docker containers if the host is a Docker host.',
      anyOf: [
        { type: 'null' },
        {
          items: {
            $comment: 'Docker Container Information\nDockerContainer',
            properties: {
              created: { $comment: 'Date/time when the container is created at.\nLocalTime', type: 'string' },
              id: { $comment: 'ID of the container..', type: 'string' },
              image: { $comment: 'Image the container is created from.', type: 'string' },
              name: { $comment: 'Name of the container.', type: 'string' },
              running: { $comment: 'Indicates if the container is running.', type: 'boolean' },
            },
            required: ['running', 'created', 'image', 'name', 'id'],
            type: 'object',
          },
          type: 'array',
        },
      ],
    },
    hardware: {
      $comment: 'Hardware information.\nRudimentary Hardware Information\nHardware',
      properties: {
        cpuCount: {
          $comment: 'Number of (v)CPU cores.',
          // maximum: 9223372036854775807,
          // minimum: -9223372036854775808,
          type: 'number',
        },
        diskRoot: { $comment: 'Total disk space of root (`/`) filesystem (in GB).', type: 'number' },
        ramTotal: { $comment: 'Total RAM (in GB).', type: 'number' },
      },
      required: ['diskRoot', 'ramTotal', 'cpuCount'],
      type: 'object',
    },
    host: {
      $comment: 'Host descriptor.\nHost Descriptor\nHost',
      properties: {
        name: { $comment: 'Name of the host.', type: 'string' },
        tags: { $comment: 'Arbitrary tags for the host.', items: { type: 'string' }, type: 'array' },
        url: { $comment: 'URL to external host information.', type: 'string' },
      },
      required: ['name'],
      type: 'object',
    },
    kernel: {
      $comment: 'Kernel information.\nKernel Information\nKernel',
      properties: {
        machine: { $comment: 'Architecture the kernel is running on (uname -m).', type: 'string' },
        name: { $comment: 'Kernel name (uname -s).', type: 'string' },
        node: { $comment: 'Name of the node kernel is running on (uname -n).', type: 'string' },
        os: { $comment: 'Operating system the kernel is driving (uname -o).', type: 'string' },
        release: { $comment: 'Kernel release (uname -r).', type: 'string' },
        version: { $comment: 'Kernel version (uname -v).', type: 'string' },
      },
      required: ['os', 'machine', 'version', 'release', 'name', 'node'],
      type: 'object',
    },
    sshAuthorizedKeys: {
      $comment: 'List of SSH authorized keys found on host.',
      items: { type: 'string' },
      type: 'array',
    },
    systemdServices: { $comment: 'List of systemd services found on host.', items: { type: 'string' }, type: 'array' },
    systemdTimers: { $comment: 'List of systemd timers found on host.', items: { type: 'string' }, type: 'array' },
  },
  required: [
    'systemdTimers',
    'systemdServices',
    'sshAuthorizedKeys',
    'dockerContainers',
    'distribution',
    'kernel',
    'hardware',
    'cloud',
    'host',
  ],
  type: 'object',
} as const satisfies JSONSchema;

export type LhpData = FromSchema<typeof LHP_PATROL_REPORT_SCHEMA>;

const AJV = new Ajv();

const LHP_PATROL_REPORT_VALIDATOR = AJV.compile<LhpData>(LHP_PATROL_REPORT_SCHEMA);

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
    const parsed = JSON.parse(raw);

    if (!Array.isArray(parsed)) {
      return Left('Data is expected to be an array of host information objects.');
    }

    for (const elem of parsed) {
      const result = LHP_PATROL_REPORT_VALIDATOR(elem);

      if (!result) {
        console.error(elem, LHP_PATROL_REPORT_VALIDATOR.errors);
        return Left('Invalid host information object.');
      }
    }

    return Right(parsed);
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
