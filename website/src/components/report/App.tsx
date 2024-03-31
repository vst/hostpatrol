import { LhpHostReport, LhpPatrolReport, buildSshKeysTable } from '@/lib/data';
import { Tab, Tabs } from '@nextui-org/react';
import { Just, Maybe, Nothing } from 'purify-ts/Maybe';
import { useEffect, useState } from 'react';
import { ShowHostDetails } from './ShowHostDetails';
import { Sidebar } from './Sidebar';
import { TabulateHosts } from './TabulateHosts';
import { TabulateSshKeys } from './TabulateSshKeys';

export function App({ data, onFlushRequest }: { data: LhpPatrolReport; onFlushRequest: () => void }) {
  const [host, setHost] = useState<Maybe<LhpHostReport>>(Nothing);
  type TabKey = 'overview' | 'tabulate-hosts' | 'show-host-details' | 'ssh-keys' | 'flush';
  const [tab, setTab] = useState<TabKey>('overview');

  useEffect(() => {
    if (host.isJust()) {
      setTab('show-host-details');
    }
  }, [host]);

  return (
    <div className="flex w-full flex-col">
      <Tabs
        aria-label="Report Views"
        fullWidth
        radius="none"
        color="secondary"
        size="lg"
        className="border-b"
        selectedKey={tab}
        onSelectionChange={(x) => {
          if (x === 'flush') {
            onFlushRequest();
          } else {
            setTab(x as TabKey);
          }
        }}
      >
        <Tab key="overview" title="🏖️ Overview" className="py-0">
          <TabOverview />
        </Tab>

        <Tab key="tabulate-hosts" title="🗒️ Tabulate Hosts" className="py-0">
          <TabTabulateHosts data={data} setHost={setHost} />
        </Tab>

        <Tab key="show-host-details" title="🔬 Host Details" className="py-0">
          <TabShowHostDetails data={data} host={host} setHost={setHost} />
        </Tab>

        <Tab key="ssh-keys" title="🛂 SSH Keys" className="py-0">
          <TabSshKeys data={data} />
        </Tab>

        <Tab key="flush" title="❌ Flush Data" className="py-0"></Tab>
      </Tabs>
    </div>
  );
}

export function TabOverview() {
  return <div>Overview is coming soon...</div>;
}

export function TabTabulateHosts({
  data,
  setHost,
}: {
  data: LhpPatrolReport;
  setHost: (x: Maybe<LhpHostReport>) => void;
}) {
  return <TabulateHosts hosts={data.hosts} onHostSelect={(x) => setHost(Just(x))} />;
}

export function TabShowHostDetails({
  data,
  host,
  setHost,
}: {
  data: LhpPatrolReport;
  host: Maybe<LhpHostReport>;
  setHost: (x: Maybe<LhpHostReport>) => void;
}) {
  return (
    <div className="grid grid-cols-6">
      <div className="border-r bg-gray-100 shadow-lg">
        <Sidebar data={data.hosts} onHostSelect={(x) => setHost(Just(x))} />
      </div>

      <div className="col-span-5">
        {host.caseOf({
          Nothing: () => <div className="p-4 text-red-400">Choose a host to view details.</div>,
          Just: (x) => <ShowHostDetails host={x} />,
        })}
      </div>
    </div>
  );
}

export function TabSshKeys({ data }: { data: LhpPatrolReport }) {
  return <TabulateSshKeys records={Object.values(buildSshKeysTable(data))} />;
}
