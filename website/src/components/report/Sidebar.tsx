import { HostReport } from '@/lib/data';
import { Listbox, ListboxItem } from '@nextui-org/listbox';
import Image from 'next/image';
import { getCloudIconName } from './helpers';

export interface SidebarProps {
  data: HostReport[];
  onHostSelect: (host: HostReport) => void;
}

export function Sidebar({ data, onHostSelect }: SidebarProps) {
  return (
    <Listbox aria-label="Sidebar" items={data} classNames={{ base: 'max-h-[80vh] overflow-y-scroll' }}>
      {/*  @ts-ignore */}
      {(host) => (
        <ListboxItem key={host.host.name} onPress={() => onHostSelect(host)}>
          <div className="flex items-center space-x-2">
            <Image
              src={`https://cdn.simpleicons.org/${getCloudIconName(host.cloud.name)}`}
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
    </Listbox>
  );
}
