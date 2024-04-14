import { HostPatrolReport, buildSshKeysTable } from '@/lib/data';
import { SimpleBarChart, histogram } from '../helpers';

export function TabOverview({ data }: { data: HostPatrolReport }) {
  const sshkeys = Object.values(buildSshKeysTable(data));

  return (
    <div className="mt-4 grid grid-cols-4 gap-4">
      <div className="col-span-4 text-center font-bold text-indigo-500">
        Showing summary of {data.hosts.length} host{data.hosts.length === 1 ? '' : 's'}, {data.knownSshKeys.length} SSH
        public key{sshkeys.length === 1 ? '' : 's'} known, and total of {sshkeys.length} SSH public key
        {sshkeys.length === 1 ? '' : 's'} seen.
      </div>

      <div>
        <SimpleBarChart
          data={histogram((x) => (x.isKnown ? 'Known SSH Keys' : 'Unknown SSH Keys'), sshkeys, {
            'Known SSH Keys': 0,
            'Unknown SSH Keys': 0,
          })}
          size={[480, 320]}
        />
      </div>

      <div>
        <SimpleBarChart
          data={histogram((x) => x.timezone.split(' ', 1)[0] || 'UNKNOWN', data.hosts)}
          size={[480, 320]}
        />
      </div>

      <div>
        <SimpleBarChart data={histogram((x) => x.cloud.name, data.hosts)} size={[480, 320]} />
      </div>

      <div>
        <SimpleBarChart data={histogram((x) => x.distribution.name, data.hosts)} size={[480, 320]} />
      </div>
    </div>
  );
}
