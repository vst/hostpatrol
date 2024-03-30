import { LhpHostReport } from '@/lib/data';
import { Chip } from '@nextui-org/chip';
import { Radio, RadioGroup, Select, SelectItem, Selection, Slider } from '@nextui-org/react';
import { Table, TableBody, TableCell, TableColumn, TableHeader, TableRow } from '@nextui-org/table';
import Image from 'next/image';
import Link from 'next/link';
import { useEffect, useState } from 'react';
import { getCloudIconName } from './helpers';

export function TabulateHosts({
  hosts,
  onHostSelect,
}: {
  hosts: LhpHostReport[];
  onHostSelect: (host: LhpHostReport) => void;
}) {
  const [filters, setFilters] = useState<Record<string, (host: LhpHostReport) => boolean>>({});
  const [filteredHosts, setFilteredHosts] = useState<LhpHostReport[]>(hosts);

  useEffect(() => {
    setFilteredHosts(hosts.filter((host) => Object.values(filters).reduce((acc, f) => acc && f(host), true)));
  }, [filters, hosts]);

  return (
    <div className="bg-white p-4">
      <div className="mb-2 grid grid-cols-5 gap-2">
        <div className="rounded-lg bg-gray-100 p-2">
          <Select
            label="Hosts"
            selectionMode="multiple"
            variant="underlined"
            className="max-w-xs"
            onSelectionChange={(x: Selection) => {
              setFilters({ ...filters, hosts: x === 'all' || x.size === 0 ? () => true : (h) => x.has(h.host.name) });
            }}
          >
            {hosts.map((host) => (
              <SelectItem key={host.host.name}>{host.host.name}</SelectItem>
            ))}
          </Select>
        </div>

        <div className="rounded-lg bg-gray-100 p-2">
          <Select
            label="Clouds"
            selectionMode="multiple"
            variant="underlined"
            className="max-w-xs"
            onSelectionChange={(x: Selection) => {
              setFilters({ ...filters, clouds: x === 'all' || x.size === 0 ? () => true : (h) => x.has(h.cloud.name) });
            }}
          >
            {hosts
              .map((h) => h.cloud.name)
              .sort()
              .filter(function (el, i, a) {
                return i === a.indexOf(el);
              })
              .map((n) => (
                <SelectItem key={n}>{n}</SelectItem>
              ))}
          </Select>
        </div>

        <div className="rounded-lg bg-gray-100 p-2">
          <Select
            label="Region"
            selectionMode="multiple"
            variant="underlined"
            className="max-w-xs"
            onSelectionChange={(x: Selection) => {
              setFilters({
                ...filters,
                region: x === 'all' || x.size === 0 ? () => true : (h) => x.has(h.cloud.hostRegion || '<unknown>'),
              });
            }}
          >
            {hosts
              .map((h) => h.cloud.hostRegion || '<unknown>')
              .sort()
              .filter(function (el, i, a) {
                return i === a.indexOf(el);
              })
              .map((n) => (
                <SelectItem key={n}>{n}</SelectItem>
              ))}
          </Select>
        </div>

        <div className="rounded-lg bg-gray-100 p-2">
          <Select
            label="Distributions"
            selectionMode="multiple"
            variant="underlined"
            className="max-w-xs"
            onSelectionChange={(x: Selection) => {
              setFilters({
                ...filters,
                distros: x === 'all' || x.size === 0 ? () => true : (h) => x.has(h.distribution.id),
              });
            }}
          >
            {hosts
              .map((h) => h.distribution.id)
              .sort()
              .filter(function (el, i, a) {
                return i === a.indexOf(el);
              })
              .map((n) => (
                <SelectItem key={n}>{n}</SelectItem>
              ))}
          </Select>
        </div>

        <div className="rounded-lg bg-gray-100 p-2">
          <Select
            label="Architectures"
            selectionMode="multiple"
            variant="underlined"
            className="max-w-xs"
            onSelectionChange={(x: Selection) => {
              setFilters({
                ...filters,
                archs: x === 'all' || x.size === 0 ? () => true : (h) => x.has(h.kernel.machine),
              });
            }}
          >
            {hosts
              .map((h) => h.kernel.machine)
              .sort()
              .filter(function (el, i, a) {
                return i === a.indexOf(el);
              })
              .map((n) => (
                <SelectItem key={n}>{n}</SelectItem>
              ))}
          </Select>
        </div>

        <div className="col-span-5 rounded-lg bg-gray-100 p-2">
          <Select
            label="Authorized SSH Keys"
            selectionMode="multiple"
            variant="underlined"
            className="max-w-full"
            onSelectionChange={(x: Selection) => {
              setFilters({
                ...filters,
                sshkeys:
                  x === 'all' || x.size === 0
                    ? () => true
                    : (h) => h.authorizedSshKeys.reduce((acc, t) => acc || x.has(t.fingerprint), false),
              });
            }}
          >
            {hosts
              .map((h) => h.authorizedSshKeys || [])
              .reduce((acc, tags) => [...acc, ...tags], [])
              .sort()
              .filter(function (el, i, a) {
                return i === a.map((x) => x.fingerprint).indexOf(el.fingerprint);
              })
              .map((n) => (
                <SelectItem
                  key={n.fingerprint}
                  title={`${n.type} ${n.length} ${n.fingerprint} - ${n.comment || 'no comment'}`}
                >
                  {n.data}
                </SelectItem>
              ))}
          </Select>
        </div>

        <div className="rounded-lg bg-gray-100 p-2">
          <Slider
            label="CPU Count"
            size="sm"
            showOutline
            step={1}
            minValue={Math.min(...hosts.map((x) => x.hardware.cpuCount))}
            maxValue={Math.max(...hosts.map((x) => x.hardware.cpuCount))}
            defaultValue={[
              Math.min(...hosts.map((x) => x.hardware.cpuCount)),
              Math.max(...hosts.map((x) => x.hardware.cpuCount)),
            ]}
            className="max-w-md"
            onChange={(x) => {
              if (Array.isArray(x)) {
                const [mi, ma] = x;
                setFilters({
                  ...filters,
                  cpu: (h) => mi <= h.hardware.cpuCount && h.hardware.cpuCount <= ma,
                });
              }
            }}
          />
        </div>

        <div className="rounded-lg bg-gray-100 p-2">
          <Slider
            label="Total RAM"
            size="sm"
            showOutline
            step={0.5}
            minValue={0}
            maxValue={Math.ceil(Math.max(...hosts.map((x) => x.hardware.ramTotal)))}
            defaultValue={[0, Math.ceil(Math.max(...hosts.map((x) => x.hardware.ramTotal)))]}
            className="max-w-md"
            onChange={(x) => {
              if (Array.isArray(x)) {
                const [mi, ma] = x;
                setFilters({
                  ...filters,
                  ram: (h) => mi <= h.hardware.ramTotal && h.hardware.ramTotal <= ma,
                });
              }
            }}
          />
        </div>

        <div className="rounded-lg bg-gray-100 p-2">
          <Slider
            label="Root Disk Size"
            size="sm"
            showOutline
            step={10}
            minValue={0}
            maxValue={Math.ceil(Math.max(...hosts.map((x) => x.hardware.diskRoot)))}
            defaultValue={[0, Math.ceil(Math.max(...hosts.map((x) => x.hardware.diskRoot)))]}
            className="max-w-md"
            onChange={(x) => {
              if (Array.isArray(x)) {
                const [mi, ma] = x;
                setFilters({
                  ...filters,
                  disk: (h) => mi <= h.hardware.diskRoot && h.hardware.diskRoot <= ma,
                });
              }
            }}
          />
        </div>

        <div className="rounded-lg bg-gray-100 p-2">
          <RadioGroup
            label="Has Docker?"
            orientation="horizontal"
            onValueChange={(x) => {
              setFilters({
                ...filters,
                docker:
                  x === 'yes'
                    ? (h) => h.dockerContainers != null
                    : x === 'no'
                      ? (h) => h.dockerContainers == null
                      : () => true,
              });
            }}
          >
            <Radio value="all">All</Radio>
            <Radio value="yes">Yes</Radio>
            <Radio value="no">No</Radio>
          </RadioGroup>
        </div>

        <div className="rounded-lg bg-gray-100 p-2">
          <Select
            label="Tags"
            selectionMode="multiple"
            variant="underlined"
            className="max-w-xs"
            onSelectionChange={(x: Selection) => {
              setFilters({
                ...filters,
                tags:
                  x === 'all' || x.size === 0
                    ? () => true
                    : (h) => (h.host.tags || []).reduce((acc, t) => acc || x.has(t), false),
              });
            }}
          >
            {hosts
              .map((h) => h.host.tags || [])
              .reduce((acc, tags) => [...acc, ...tags], [])
              .sort()
              .filter(function (el, i, a) {
                return i === a.indexOf(el);
              })
              .map((n) => (
                <SelectItem key={n}>{n}</SelectItem>
              ))}
          </Select>
        </div>
      </div>

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
          <TableColumn key="sshkeys" align="end">
            SSH Keys
          </TableColumn>
          <TableColumn key="systemd" align="end">
            Systemd
          </TableColumn>
          <TableColumn key="tags">Tags</TableColumn>
        </TableHeader>
        <TableBody items={filteredHosts}>
          {(host) => (
            <TableRow key={host.host.name}>
              <TableCell>
                <div className="flex items-center space-x-2">
                  <Image
                    src={`https://cdn.simpleicons.org/${getCloudIconName(host.cloud.name)}`}
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
              <TableCell>{host.authorizedSshKeys.length}</TableCell>
              <TableCell>
                {host.systemdServices.length} / {host.systemdTimers.length}
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
