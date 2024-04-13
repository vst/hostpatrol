import { Card, CardBody, CardHeader } from '@nextui-org/card';
import { Listbox, ListboxItem } from '@nextui-org/listbox';
import { Spinner } from '@nextui-org/react';
import { Bar, BarChart, CartesianGrid, Cell, Pie, PieChart, Tooltip, XAxis, YAxis } from 'recharts';

export function Centered({ children }: { children: React.ReactNode }) {
  return <div className="flex flex-grow items-center justify-center">{children}</div>;
}

export function BigSpinner({ label }: { label?: string }) {
  return (
    <Centered>
      <Spinner label={label} size="lg" color="default" />
    </Centered>
  );
}

export function KVBox({
  title,
  kvs,
  ...rest
}: {
  title: string;
  kvs: { key: string; value: React.ReactNode | string | number | null | undefined }[];
  [prop: string]: any;
}) {
  return (
    <Card radius="sm" shadow="sm" {...rest}>
      <CardHeader className="text-lg font-bold">{title}</CardHeader>

      <CardBody>
        <Listbox items={kvs}>
          {({ key, value }) => (
            <ListboxItem key={key} endContent={value}>
              {key}
            </ListboxItem>
          )}
        </Listbox>
      </CardBody>
    </Card>
  );
}

export const CHART_COLORS_RETRO = [
  '#ea5545',
  '#f46a9b',
  '#ef9b20',
  '#edbf33',
  '#ede15b',
  '#bdcf32',
  '#87bc45',
  '#27aeef',
  '#b33dc6',
];

export const CHART_COLORS_SPRING = [
  '#fd7f6f',
  '#7eb0d5',
  '#b2e061',
  '#bd7ebe',
  '#ffb55a',
  '#ffee65',
  '#beb9db',
  '#fdcce5',
  '#8bd3c7',
];

export const CHART_COLORS_RIVER = [
  '#b30000',
  '#7c1158',
  '#4421af',
  '#1a53ff',
  '#0d88e6',
  '#00b7c7',
  '#5ad45a',
  '#8be04e',
  '#ebdc78',
];

export const CHART_COLORS = CHART_COLORS_SPRING;

export function SimpleBarChart({ data, size }: { data: Record<string, number>; size: [number, number] }) {
  const [width, height] = size;
  const observations = Object.entries(data).map(([name, value]) => ({ name, value }));

  return (
    <BarChart width={width} height={height} data={observations} margin={{ top: 5, right: 30, left: 20, bottom: 5 }}>
      <CartesianGrid strokeDasharray="3 3" />
      <XAxis dataKey="name" />
      <YAxis />
      <Tooltip />
      <Bar dataKey="value" fill="#8884d8">
        {observations.map((_entry, index) => (
          <Cell key={`cell-${index}`} fill={CHART_COLORS[index % CHART_COLORS.length]} />
        ))}
      </Bar>
    </BarChart>
  );
}

export function SimplePieChart({ data, size }: { data: Record<string, number>; size: [number, number] }) {
  const [width, height] = size;
  const observations = Object.entries(data).map(([name, value]) => ({ name, value }));

  return (
    <PieChart width={width} height={height}>
      <Pie dataKey="value" data={observations} cx="50%" cy="50%" outerRadius={80} fill="#88b84d8" label>
        {observations.map((_entry, index) => (
          <Cell key={`cell-${index}`} fill={CHART_COLORS[index % CHART_COLORS.length]} />
        ))}
      </Pie>
      <Tooltip />
    </PieChart>
  );
}

export function histogram<R>(f: (r: R) => string, rs: R[], init: Record<string, number> = {}): Record<string, number> {
  return rs.map(f).reduce((acc, x) => ({ ...acc, [x]: (acc[x] || 0) + 1 }), init);
}
