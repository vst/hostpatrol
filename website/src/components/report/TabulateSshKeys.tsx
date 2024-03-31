import { SshKeysTableRecord } from '@/lib/data';
import { Chip } from '@nextui-org/chip';
import { Radio, RadioGroup, Select, SelectItem, Selection } from '@nextui-org/react';
import { Table, TableBody, TableCell, TableColumn, TableHeader, TableRow } from '@nextui-org/table';
import { useEffect, useState } from 'react';

export function TabulateSshKeys({ records }: { records: SshKeysTableRecord[] }) {
  const [filters, setFilters] = useState<Record<string, (record: SshKeysTableRecord) => boolean>>({});
  const [filteredRecords, setFilteredRecords] = useState<SshKeysTableRecord[]>(records);

  useEffect(() => {
    setFilteredRecords(records.filter((k) => Object.values(filters).reduce((acc, f) => acc && f(k), true)));
  }, [filters, records]);

  return (
    <div className="bg-white p-4">
      <div className="mb-2 grid grid-cols-5 gap-2">
        <div className="rounded-lg bg-gray-100 p-2">
          <Select
            label="Types"
            selectionMode="multiple"
            variant="underlined"
            onSelectionChange={(x: Selection) => {
              setFilters({ ...filters, types: x === 'all' || x.size === 0 ? () => true : (r) => x.has(r.key.type) });
            }}
          >
            {records
              .map((r) => r.key.type)
              .sort()
              .filter(function (el, i, a) {
                return i === a.indexOf(el);
              })
              .map((v) => (
                <SelectItem key={v}>{v}</SelectItem>
              ))}
          </Select>
        </div>

        <div className="rounded-lg bg-gray-100 p-2">
          <RadioGroup
            label="Is Known?"
            orientation="horizontal"
            onValueChange={(x) => {
              setFilters({
                ...filters,
                docker: x === 'yes' ? (r) => r.isKnown : x === 'no' ? (r) => !r.isKnown : () => true,
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
            label="Known As"
            selectionMode="multiple"
            variant="underlined"
            onSelectionChange={(x: Selection) => {
              setFilters({
                ...filters,
                knownas: x === 'all' || x.size === 0 ? () => true : (r) => r.isKnown && x.has(r.knownComment),
              });
            }}
          >
            {records
              .filter((r) => r.isKnown)
              .map((r) => r.knownComment)
              .sort()
              .filter(function (el, i, a) {
                return i === a.indexOf(el);
              })
              .map((v) => (
                <SelectItem key={v}>{v}</SelectItem>
              ))}
          </Select>
        </div>

        <div className="rounded-lg bg-gray-100 p-2">
          <Select
            label="Hosts"
            selectionMode="multiple"
            variant="underlined"
            onSelectionChange={(x: Selection) => {
              setFilters({
                ...filters,
                hosts:
                  x === 'all' || x.size === 0
                    ? () => true
                    : (r) => Array.from(r.seenHosts).reduce((acc, c) => acc || x.has(c.host.name), false),
              });
            }}
          >
            {records
              .map((r) => Array.from(r.seenHosts).map((x) => x.host.name))
              .reduce((acc, r) => [...acc, ...r], [])
              .sort()
              .filter(function (el, i, a) {
                return i === a.indexOf(el);
              })
              .map((v) => (
                <SelectItem key={v}>{v}</SelectItem>
              ))}
          </Select>
        </div>

        <div className="rounded-lg bg-gray-100 p-2">
          <Select
            label="Seen Comments"
            selectionMode="multiple"
            variant="underlined"
            onSelectionChange={(x: Selection) => {
              setFilters({
                ...filters,
                seencomments:
                  x === 'all' || x.size === 0
                    ? () => true
                    : (r) => Array.from(r.seenComments).reduce((acc, c) => acc || x.has(c), false),
              });
            }}
          >
            {records
              .map((r) => Array.from(r.seenComments))
              .reduce((acc, r) => [...acc, ...r], [])
              .sort()
              .filter(function (el, i, a) {
                return i === a.indexOf(el);
              })
              .map((v) => (
                <SelectItem key={v}>{v}</SelectItem>
              ))}
          </Select>
        </div>
      </div>

      <Table
        aria-label="Table of SSH Keys"
        removeWrapper
        selectionMode="multiple"
        color="secondary"
        showSelectionCheckboxes={false}
      >
        <TableHeader>
          <TableColumn key="type">Type</TableColumn>
          <TableColumn key="length">Length</TableColumn>
          <TableColumn key="known">Known As</TableColumn>
          <TableColumn key="fingerprint">Fingerprint</TableColumn>
          <TableColumn key="seen-hosts-count">Hosts Count</TableColumn>
          <TableColumn key="seen-hosts">Seen on Hosts</TableColumn>
          <TableColumn key="seen-comments">Seen with Comments</TableColumn>
        </TableHeader>

        <TableBody items={filteredRecords}>
          {(record) => (
            <TableRow key={record.key.fingerprint}>
              <TableCell> {record.key.type} </TableCell>
              <TableCell> {record.key.length} </TableCell>
              <TableCell>
                {record.isKnown ? (
                  <Chip color="success">{record.knownComment}</Chip>
                ) : (
                  <Chip color="danger">UNKNOWN</Chip>
                )}
              </TableCell>
              <TableCell>{record.key.fingerprint}</TableCell>
              <TableCell>{record.seenHosts.size}</TableCell>
              <TableCell>
                {Array.from(record.seenHosts).map((h) => (
                  <Chip key={h.host.name}>{h.host.name}</Chip>
                ))}
              </TableCell>
              <TableCell>
                {Array.from(record.seenComments).map((c) => (
                  <Chip key={c}>{c}</Chip>
                ))}
              </TableCell>
            </TableRow>
          )}
        </TableBody>
      </Table>
    </div>
  );
}
