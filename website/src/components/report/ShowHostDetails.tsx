import { LhpHostReport } from '@/lib/data';
import { Card, CardBody, CardHeader } from '@nextui-org/card';
import { Chip } from '@nextui-org/chip';
import { Listbox, ListboxItem } from '@nextui-org/listbox';
import Link from 'next/link';
import { toast } from 'react-toastify';
import { KVBox } from '../helpers';

export function ShowHostDetails({ host }: { host: LhpHostReport }) {
  return (
    <div className="space-y-4 px-4 py-4">
      <h1 className="flex flex-row justify-between text-xl font-bold">
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
        <CardHeader className="text-lg font-bold">Authorized SSH Keys</CardHeader>

        <CardBody>
          <Listbox
            items={host.authorizedSshKeys}
            emptyContent={<span className="text-orange-400">No authorized SSH keys are found. Sounds weird?</span>}
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
    </div>
  );
}
