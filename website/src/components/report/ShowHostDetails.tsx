import { HostPatrolReport, HostReport, SshPublicKey } from '@/lib/data';
import {
  Card,
  CardBody,
  CardFooter,
  CardHeader,
  Chip,
  Listbox,
  ListboxItem,
  Table,
  TableBody,
  TableCell,
  TableColumn,
  TableHeader,
  TableRow,
} from '@nextui-org/react';
import Link from 'next/link';
import { toast } from 'react-toastify';
import { KVBox } from '../helpers';

export function ShowHostDetails({ host, data }: { host: HostReport; data: HostPatrolReport }) {
  return (
    <div className="space-y-4 px-4 py-4">
      <h1 className="flex flex-row items-center justify-between text-xl font-bold">
        <div className="space-x-2">
          <span>{host.host.name}</span>
          {host.host.url && (
            <Link href={host.host.url} target="_blank">
              🔗
            </Link>
          )}
          {host.host.id && (
            <Chip
              onClick={() => {
                navigator.clipboard.writeText(host.host.id || '');
                toast('External host identifier is copied to clipboard.');
              }}
            >
              <span className="cursor-pointer">{host.host.id}</span>
            </Chip>
          )}
        </div>

        <div className="align-items flex flex-row items-center space-x-4">
          <div className="text-xs font-medium text-gray-500">
            {host.hostname} on {host.timezone}
          </div>

          <div className="space-x-1 pb-1">
            {(host.host.tags || []).map((x) => (
              <Chip key={x} size="sm" color="primary" variant="flat" radius="sm">
                {x}
              </Chip>
            ))}
          </div>
        </div>
      </h1>

      <div className="grid grid-cols-1 gap-4 lg:grid-cols-3">
        <KVBox
          title="Hardware"
          kvs={[
            { key: 'CPU', value: host.hardware.cpuCount },
            { key: 'Memory', value: host.hardware.ramTotal },
            { key: 'Disk', value: host.hardware.diskRoot },
          ]}
        />

        <KVBox
          title="Cloud"
          kvs={[
            { key: 'Name', value: host.cloud.name },
            { key: 'ID', value: host.cloud.id },
            { key: 'Type', value: host.cloud.hostType },
            { key: 'Region', value: host.cloud.hostRegion },
            { key: 'Availability Zone', value: host.cloud.hostAvailabilityZone },
            { key: 'Local Hostname', value: host.cloud.hostLocalHostname },
            { key: 'Local Address', value: host.cloud.hostLocalAddress },
            { key: 'Remote Hostname', value: host.cloud.hostRemoteHostname },
            { key: 'Remote Address', value: host.cloud.hostRemoteAddress },
            { key: 'Reserved Address', value: host.cloud.hostReservedAddress },
          ]}
          className="lg:col-span-2"
        />
      </div>

      <div className="grid grid-cols-1 gap-4 lg:grid-cols-2">
        <KVBox
          title="Distribution"
          kvs={[
            { key: 'ID', value: host.distribution.id },
            { key: 'Name', value: host.distribution.name },
            { key: 'Description', value: host.distribution.description },
            { key: 'Release', value: host.distribution.release },
            { key: 'Version', value: host.distribution.version },
            { key: 'Codename', value: host.distribution.codename },
          ]}
        />

        <KVBox
          title="Kernel"
          kvs={[
            { key: 'Node', value: host.kernel.node },
            { key: 'Name', value: host.kernel.name },
            { key: 'Machine', value: host.kernel.machine },
            { key: 'Release', value: host.kernel.release },
            { key: 'Version', value: host.kernel.version },
            { key: 'Operating System', value: host.kernel.os },
          ]}
        />
      </div>

      <Card radius="sm" shadow="sm">
        <CardHeader className="text-lg font-bold">Authorized SSH Public Keys</CardHeader>

        <CardBody>
          <TabulateSshKeys host={host} data={data} />
        </CardBody>

        <CardFooter>
          <span
            className="mr-2 cursor-pointer"
            onClick={() => {
              const keys = Object.values(
                [...(host.host.knownSshKeys || []), ...(data.knownSshKeys || [])].reduce(
                  (acc, x) => ({ ...acc, [`${x.fingerprint}`]: x }),
                  {} as Record<string, SshPublicKey>
                )
              ).sort((a, b) => (a.comment || '').localeCompare(b.comment || ''));

              navigator.clipboard.writeText(keys.map((x) => `${x.data} ${x.comment}`).join('\n'));
              toast('SSH public keys are copied to clipboard.');
            }}
          >
            (copy known keys)
          </span>
        </CardFooter>
      </Card>

      <Card radius="sm" shadow="sm">
        <CardHeader className="text-lg font-bold">Public SSH Host Keys</CardHeader>

        <CardBody>
          <Listbox
            items={host.publicSshHostKeys}
            emptyContent={<span className="text-orange-400">No public SSH host keys are found. Sounds weird?</span>}
          >
            {({ length, type, fingerprint, data, comment }) => (
              <ListboxItem
                key={data}
                description={data}
                onPress={() => {
                  navigator.clipboard.writeText(data);
                  toast('SSH Key is copied to clipboard.');
                }}
              >
                {`${type} (${length}) - ${fingerprint} - ${comment || ''}`}
              </ListboxItem>
            )}
          </Listbox>
        </CardBody>
      </Card>

      <Card radius="sm" shadow="sm">
        <CardHeader className="text-lg font-bold">Docker Containers</CardHeader>

        <CardBody>
          {host.dockerContainers ? (
            <Listbox
              items={[
                ...host.dockerContainers.sort(
                  (a, b) => (a.running ? 0 : 1) - (b.running ? 0 : 1) || a.name.localeCompare(b.name)
                ),
              ]}
              emptyContent={<span className="text-orange-400">Docker service has no containers.</span>}
            >
              {({ id, image, name, running, created }) => (
                <ListboxItem
                  key={id}
                  description={image}
                  startContent={running ? <>🟢</> : <>🔴</>}
                  endContent={
                    <span className="text-xs" title="Created">
                      {created}
                    </span>
                  }
                >
                  {name}
                </ListboxItem>
              )}
            </Listbox>
          ) : (
            <span className="text-red-400">Docker service is not found.</span>
          )}
        </CardBody>
      </Card>

      <div className="grid grid-cols-1 gap-4 lg:grid-cols-2">
        <KVBox title="Systemd Services" kvs={host.systemdServices.map((x) => ({ key: x, value: '✅' }))} />

        <KVBox title="Systemd Timers" kvs={host.systemdTimers.map((x) => ({ key: x, value: '✅' }))} />
      </div>

      <Card radius="sm" shadow="sm">
        <CardHeader className="text-lg font-bold">Extra Host Data</CardHeader>

        <CardBody>
          <pre>{host.host.data ? JSON.stringify(host.host.data || '#N/A', null, 2) : '#N/A'}</pre>
        </CardBody>
      </Card>
    </div>
  );
}

export function TabulateSshKeys({ host, data }: { host: HostReport; data: HostPatrolReport }) {
  const keysKnownGlobal = (data.knownSshKeys || []).reduce(
    (acc, x) => ({ ...acc, [`${x.fingerprint}`]: x }),
    {} as Record<string, SshPublicKey>
  );
  const keysKnownHost = (host.host.knownSshKeys || []).reduce(
    (acc, x) => ({ ...acc, [`${x.fingerprint}`]: x }),
    {} as Record<string, SshPublicKey>
  );
  const keysSeen = (host.authorizedSshKeys || []).reduce(
    (acc, x) => ({ ...acc, [`${x.fingerprint}`]: x }),
    {} as Record<string, SshPublicKey>
  );
  const keys = Object.values(keysKnownGlobal).concat(Object.values(keysKnownHost), Object.values(keysSeen));
  const fps = Array.from(new Set(keys.map((x) => x.fingerprint))).map((fingerprint) => ({ fingerprint }));

  return (
    <Table aria-label="Table of SSH Keys" removeWrapper color="secondary" showSelectionCheckboxes={false}>
      <TableHeader>
        <TableColumn key="problem">Problem?</TableColumn>
        <TableColumn key="type">Type</TableColumn>
        <TableColumn key="length">Length</TableColumn>
        <TableColumn key="known">Known?</TableColumn>
        <TableColumn key="seen">Seen?</TableColumn>
        <TableColumn key="fingerprint">Fingerprint</TableColumn>
        <TableColumn key="seen-comments">Comments</TableColumn>
      </TableHeader>

      <TableBody items={fps}>
        {(record) => {
          const fp = record.fingerprint;

          const keyG = keysKnownGlobal[fp];
          const keyH = keysKnownHost[fp];
          const keyS = keysSeen[fp];
          const key = (keyG || keyH || keyS) as SshPublicKey;
          const known: 'global' | 'host' | 'unknown' = keyG ? 'global' : keyH ? 'host' : 'unknown';
          const seen = keyS ? true : false;
          const problem = (known === 'unknown' && seen) || ((known === 'host' || known === 'global') && !seen);
          const comments = Array.from(new Set(keys.filter((x) => x.fingerprint === fp).map((x) => x.comment)));

          return (
            <TableRow key={fp}>
              <TableCell>{problem ? '🔴' : '🟢'}</TableCell>
              <TableCell>{key.type}</TableCell>
              <TableCell>{key.length}</TableCell>
              <TableCell>
                <Chip color={known === 'global' ? 'success' : known === 'host' ? 'primary' : 'danger'}>{known}</Chip>
              </TableCell>
              <TableCell>
                <Chip color={seen ? 'success' : 'danger'}>{seen ? 'yes' : 'no'}</Chip>
              </TableCell>
              <TableCell>
                {key.fingerprint}
                <div>
                  <span
                    className="mr-2 cursor-pointer"
                    onClick={() => {
                      navigator.clipboard.writeText(key.fingerprint);
                      toast('SSH public key fingerprint is copied to clipboard.');
                    }}
                  >
                    (copy fingerprint)
                  </span>

                  <span
                    className="cursor-pointer"
                    onClick={() => {
                      navigator.clipboard.writeText(key.data);
                      toast('SSH public key is copied to clipboard.');
                    }}
                  >
                    (copy key)
                  </span>
                </div>
              </TableCell>
              <TableCell>
                {comments.map((c) => (
                  <Chip key={c}>{c}</Chip>
                ))}
              </TableCell>
            </TableRow>
          );
        }}
      </TableBody>
    </Table>
  );
}
