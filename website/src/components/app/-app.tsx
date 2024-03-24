import { Card, CardBody, CardHeader } from '@nextui-org/card';
import { Chip } from '@nextui-org/chip';
import { Listbox, ListboxItem, ListboxSection } from '@nextui-org/listbox';
import { Table, TableBody, TableCell, TableColumn, TableHeader, TableRow } from '@nextui-org/table';
import Image from 'next/image';
import Link from 'next/link';
import { Just, Maybe, Nothing } from 'purify-ts/Maybe';
import { useState } from 'react';
import { LhpData } from './-data';
import { KVBox } from './-ui';

export function App({ data, onFlushRequest }: { data: LhpData[]; onFlushRequest: () => void }) {
  const [host, setHost] = useState<Maybe<LhpData>>(Nothing);

  return (
    <div className="flex flex-1">
      <div className="z-10 w-[300px] shadow-md">
        <Sidebar
          data={data}
          onHostSelect={(x) => setHost(Just(x))}
          onFlushRequest={onFlushRequest}
          onTabulateRequest={() => {
            setHost(Nothing);
          }}
        />
      </div>

      <div className="w-full">
        {host.caseOf({
          Nothing: () => <TabulateHosts hosts={data} onHostSelect={(x) => setHost(Just(x))} />,
          Just: (x) => <HostDetails host={x} />,
        })}
      </div>
    </div>
  );
}

export interface SidebarProps {
  data: LhpData[];
  onHostSelect: (host: LhpData) => void;
  onFlushRequest: () => void;
  onTabulateRequest: () => void;
}

export function Sidebar({ data, onHostSelect, onTabulateRequest, onFlushRequest }: SidebarProps) {
  return (
    <Listbox aria-label="Sidebar">
      <ListboxSection title="Summaries">
        <ListboxItem key="summary-tabulate" onPress={() => onTabulateRequest()}>
          Tabulate
        </ListboxItem>
      </ListboxSection>

      <ListboxSection title="Hosts" items={data}>
        {/*  @ts-ignore */}
        {(host) => (
          <ListboxItem key={host.host.name} onPress={() => onHostSelect(host)}>
            <div className="flex items-center space-x-2">
              <Image
                src={`https://cdn.simpleicons.org/${cloudIcon(host.cloud.name)}`}
                width="16"
                height="16"
                alt={`logo ${host.cloud.name}`}
                unoptimized
              />
              <Image
                src={`https://cdn.simpleicons.org/${host.distribution.id}`}
                width="16"
                height="16"
                alt={`logo ${host.distribution.id}`}
                unoptimized
              />
              <span>{host.host.name}</span>
            </div>
          </ListboxItem>
        )}
      </ListboxSection>

      <ListboxSection title="Actions">
        <ListboxItem key="action-flush" onPress={() => onFlushRequest()}>
          Flush Data
        </ListboxItem>
      </ListboxSection>
    </Listbox>
  );
}

export function cloudIcon(x: string) {
  switch (x.toLowerCase()) {
    case 'aws':
      return 'amazonec2';
    case 'do':
      return 'digitalocean';
    case 'hetzner':
      return 'hetzner';
    default:
      return 'educative';
  }
}

export function TabulateHosts({ hosts, onHostSelect }: { hosts: LhpData[]; onHostSelect: (host: LhpData) => void }) {
  return (
    <div className="bg-white p-4">
      <Table
        aria-label="Table of Hosts"
        removeWrapper
        selectionMode="multiple"
        color="secondary"
        showSelectionCheckboxes={false}
      >
        <TableHeader>
          <TableColumn key="hostname">Hostname</TableColumn>
          <TableColumn key="cloud">Cloud</TableColumn>
          <TableColumn key="distro">Distribution</TableColumn>
          <TableColumn key="arch">Arch</TableColumn>
          <TableColumn key="cpu" align="end">
            CPU
          </TableColumn>
          <TableColumn key="ram" align="end">
            RAM
          </TableColumn>
          <TableColumn key="disk" align="end">
            Disk
          </TableColumn>
          <TableColumn key="docker" align="center">
            Docker
          </TableColumn>
          <TableColumn key="tags">Tags</TableColumn>
        </TableHeader>
        <TableBody items={hosts}>
          {(host) => (
            <TableRow key={host.host.name}>
              <TableCell>
                <div className="flex items-center space-x-2">
                  <Image
                    src={`https://cdn.simpleicons.org/${cloudIcon(host.cloud.name)}`}
                    width="24"
                    height="24"
                    alt={`logo ${host.cloud.name}`}
                    unoptimized
                  />
                  <Image
                    src={`https://cdn.simpleicons.org/${host.distribution.id}`}
                    width="24"
                    height="24"
                    alt={`logo ${host.distribution.id}`}
                    unoptimized
                  />
                  <span className="cursor-pointer" onClick={() => onHostSelect(host)}>
                    {host.host.name}
                  </span>
                  {host.host.url && (
                    <Link href={host.host.url} target="_blank">
                      🔗
                    </Link>
                  )}
                </div>
              </TableCell>
              <TableCell>
                {host.cloud.name}
                {host.cloud.hostRegion && <span className="text-xs text-gray-400"> {host.cloud.hostRegion}</span>}
              </TableCell>
              <TableCell>{host.distribution.description}</TableCell>
              <TableCell>{host.kernel.machine}</TableCell>
              <TableCell>{host.hardware.cpuCount}</TableCell>
              <TableCell>{host.hardware.ramTotal}</TableCell>
              <TableCell>{host.hardware.diskRoot}</TableCell>
              <TableCell>
                {host.dockerContainers == null
                  ? '❌'
                  : `${host.dockerContainers.filter((x) => x.running).length} / ${host.dockerContainers.length}`}
              </TableCell>
              <TableCell className="space-x-1">
                {(host.host.tags || []).map((x) => (
                  <Chip key={x} size="sm" color="primary" variant="flat" radius="sm">
                    {x}
                  </Chip>
                ))}
              </TableCell>
            </TableRow>
          )}
        </TableBody>
      </Table>
    </div>
  );
}

export function HostDetails({ host }: { host: LhpData }) {
  return (
    <div>
      <h1 className="flex flex-row justify-between border-b border-gray-200 bg-white p-4 text-xl font-bold">
        <div className="space-x-2">
          <span>{host.host.name}</span>
          {host.host.url && (
            <Link href={host.host.url} target="_blank">
              🔗
            </Link>
          )}
        </div>

        <div className="space-x-1">
          {(host.host.tags || []).map((x) => (
            <Chip key={x} size="sm" color="primary" variant="flat" radius="sm">
              {x}
            </Chip>
          ))}
        </div>
      </h1>

      <div className="p-4">
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
        />
      </div>

      <div className="grid grid-cols-1 gap-4 p-4 lg:grid-cols-2">
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

      <div className="p-4">
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
      </div>
    </div>
  );
}
