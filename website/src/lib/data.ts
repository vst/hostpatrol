import Ajv from 'ajv';
import type { JSONSchema } from 'json-schema-to-ts';
import { FromSchema } from 'json-schema-to-ts';
import { Either, Left, Right } from 'purify-ts/Either';
import { Just, Maybe, Nothing } from 'purify-ts/Maybe';

export const HOSTPATROL_REPORT_SCHEMA = {
  $comment: 'Host Patrol Report\nReport',
  properties: {
    errors: {
      $comment: 'List of errors encountered during the report generation.',
      items: {
        $comment: 'Report Error\nReportError',
        properties: {
          host: {
            $comment: 'Host of the error if applicable.',
            type: 'string',
          },
          message: {
            $comment: 'Error message.',
            type: 'string',
          },
        },
        required: ['message'],
        type: 'object',
      },
      type: 'array',
    },
    hosts: {
      $comment: 'List of host reports.',
      items: {
        $comment: 'Host Patrol Report\nReport',
        properties: {
          authorizedSshKeys: {
            $comment: 'List of authorized SSH public keys found on host.',
            items: {
              $comment: 'SSH Public Key Information\nSshPublicKey',
              properties: {
                comment: {
                  $comment: 'Comment on the public key.',
                  type: 'string',
                },
                data: {
                  $comment: 'Original information.',
                  type: 'string',
                },
                fingerprint: {
                  $comment: 'Fingerprint of the public key.',
                  type: 'string',
                },
                length: {
                  $comment: 'Length of the public key.',
                  maximum: 2147483647,
                  minimum: -2147483648,
                  type: 'number',
                },
                type: {
                  $comment: 'Type of the public key.',
                  type: 'string',
                },
              },
              required: ['fingerprint', 'comment', 'length', 'type', 'data'],
              type: 'object',
            },
            type: 'array',
          },
          clock: {
            $comment: 'Clock information.\nClock Information\nClock',
            properties: {
              ntp_availability: {
                $comment: 'Indicates NTP availability and enablement.',
                type: 'string',
              },
              time_sync_status: {
                $comment: 'Indicates time synchronisation status.',
                type: 'string',
              },
            },
            required: ['time_sync_status', 'ntp_availability'],
            type: 'object',
          },
          cloud: {
            $comment: 'Cloud information.\nCloud Information\nCloud',
            properties: {
              hostAvailabilityZone: {
                $comment: 'Host availability zone.',
                anyOf: [
                  {
                    type: 'null',
                  },
                  {
                    type: 'string',
                  },
                ],
              },
              hostLocalAddress: {
                $comment: 'Local address of the host.',
                anyOf: [
                  {
                    type: 'null',
                  },
                  {
                    type: 'string',
                  },
                ],
              },
              hostLocalHostname: {
                $comment: 'Local hostname of the host.',
                anyOf: [
                  {
                    type: 'null',
                  },
                  {
                    type: 'string',
                  },
                ],
              },
              hostRegion: {
                $comment: 'Host region.',
                anyOf: [
                  {
                    type: 'null',
                  },
                  {
                    type: 'string',
                  },
                ],
              },
              hostRemoteAddress: {
                $comment: 'Remote address of the host.',
                anyOf: [
                  {
                    type: 'null',
                  },
                  {
                    type: 'string',
                  },
                ],
              },
              hostRemoteHostname: {
                $comment: 'Remote hostname of the host.',
                anyOf: [
                  {
                    type: 'null',
                  },
                  {
                    type: 'string',
                  },
                ],
              },
              hostReservedAddress: {
                $comment: 'Reserved address of the host.',
                anyOf: [
                  {
                    type: 'null',
                  },
                  {
                    type: 'string',
                  },
                ],
              },
              hostType: {
                $comment: 'Host type.',
                anyOf: [
                  {
                    type: 'null',
                  },
                  {
                    type: 'string',
                  },
                ],
              },
              id: {
                $comment: 'Host identifier.',
                anyOf: [
                  {
                    type: 'null',
                  },
                  {
                    type: 'string',
                  },
                ],
              },
              name: {
                $comment: 'Cloud name.',
                type: 'string',
              },
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
                anyOf: [
                  {
                    type: 'null',
                  },
                  {
                    type: 'string',
                  },
                ],
              },
              description: {
                $comment: "Distribution description (cat /etc/os-release | grep 'PRETTY_NAME=').",
                type: 'string',
              },
              id: {
                $comment: "Distribution ID (cat /etc/os-release | grep 'ID=').",
                type: 'string',
              },
              name: {
                $comment: "Distribution name (cat /etc/os-release | grep 'NAME=')).",
                type: 'string',
              },
              release: {
                $comment: "Distribution release (cat /etc/os-release | grep 'VERSION_ID=').",
                type: 'string',
              },
              version: {
                $comment: "Distribution version (cat /etc/os-release | grep 'VERSION=').",
                type: 'string',
              },
            },
            required: ['description', 'codename', 'release', 'version', 'name', 'id'],
            type: 'object',
          },
          dockerContainers: {
            $comment: 'List of Docker containers if the host is a Docker host.',
            anyOf: [
              {
                type: 'null',
              },
              {
                items: {
                  $comment: 'Docker Container Information\nDockerContainer',
                  properties: {
                    created: {
                      $comment: 'Date/time when the container is created at.\nDate/time in ISO8601 format.',
                      type: 'string',
                    },
                    id: {
                      $comment: 'ID of the container..',
                      type: 'string',
                    },
                    image: {
                      $comment: 'Image the container is created from.',
                      type: 'string',
                    },
                    name: {
                      $comment: 'Name of the container.',
                      type: 'string',
                    },
                    running: {
                      $comment: 'Indicates if the container is running.',
                      type: 'boolean',
                    },
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
                maximum: 2147483647,
                minimum: -2147483648,
                type: 'number',
              },
              diskRoot: {
                $comment: 'Total disk space of root (`/`) filesystem (in GB).',
                type: 'number',
              },
              ramTotal: {
                $comment: 'Total RAM (in GB).',
                type: 'number',
              },
            },
            required: ['diskRoot', 'ramTotal', 'cpuCount'],
            type: 'object',
          },
          host: {
            $comment: 'Host descriptor.\nHost Descriptor\nHost',
            properties: {
              data: {
                $comment: 'Arbitrary data for the host.',
              },
              id: {
                $comment: 'External identifier of the host.',
                type: 'string',
              },
              knownSshKeys: {
                $comment: 'Known SSH public keys for the host.',
                items: {
                  $comment: 'SSH Public Key Information\nSshPublicKey',
                  properties: {
                    comment: {
                      $comment: 'Comment on the public key.',
                      type: 'string',
                    },
                    data: {
                      $comment: 'Original information.',
                      type: 'string',
                    },
                    fingerprint: {
                      $comment: 'Fingerprint of the public key.',
                      type: 'string',
                    },
                    length: {
                      $comment: 'Length of the public key.',
                      maximum: 2147483647,
                      minimum: -2147483648,
                      type: 'number',
                    },
                    type: {
                      $comment: 'Type of the public key.',
                      type: 'string',
                    },
                  },
                  required: ['fingerprint', 'comment', 'length', 'type', 'data'],
                  type: 'object',
                },
                type: 'array',
              },
              name: {
                $comment: 'Name of the host.',
                type: 'string',
              },
              ssh: {
                $comment: 'SSH configuration.\nSSH Configuration\nSshConfig',
                properties: {
                  destination: {
                    $comment: 'SSH destination.',
                    type: 'string',
                  },
                  options: {
                    $comment: 'SSH options.',
                    items: {
                      type: 'string',
                    },
                    type: 'array',
                  },
                },
                required: ['destination'],
                type: 'object',
              },
              tags: {
                $comment: 'Arbitrary tags for the host.',
                items: {
                  type: 'string',
                },
                type: 'array',
              },
              url: {
                $comment: 'URL to external host information.',
                type: 'string',
              },
            },
            required: ['name'],
            type: 'object',
          },
          hostname: {
            $comment: 'Hostname of the host.',
            type: 'string',
          },
          kernel: {
            $comment: 'Kernel information.\nKernel Information\nKernel',
            properties: {
              machine: {
                $comment: 'Architecture the kernel is running on (uname -m).',
                type: 'string',
              },
              name: {
                $comment: 'Kernel name (uname -s).',
                type: 'string',
              },
              node: {
                $comment: 'Name of the node kernel is running on (uname -n).',
                type: 'string',
              },
              os: {
                $comment: 'Operating system the kernel is driving (uname -o).',
                type: 'string',
              },
              release: {
                $comment: 'Kernel release (uname -r).',
                type: 'string',
              },
              version: {
                $comment: 'Kernel version (uname -v).',
                type: 'string',
              },
            },
            required: ['os', 'machine', 'version', 'release', 'name', 'node'],
            type: 'object',
          },
          publicSshHostKeys: {
            $comment: 'List of public SSH host keys found on host.',
            items: {
              $comment: 'SSH Public Key Information\nSshPublicKey',
              properties: {
                comment: {
                  $comment: 'Comment on the public key.',
                  type: 'string',
                },
                data: {
                  $comment: 'Original information.',
                  type: 'string',
                },
                fingerprint: {
                  $comment: 'Fingerprint of the public key.',
                  type: 'string',
                },
                length: {
                  $comment: 'Length of the public key.',
                  maximum: 2147483647,
                  minimum: -2147483648,
                  type: 'number',
                },
                type: {
                  $comment: 'Type of the public key.',
                  type: 'string',
                },
              },
              required: ['fingerprint', 'comment', 'length', 'type', 'data'],
              type: 'object',
            },
            type: 'array',
          },
          systemdServices: {
            $comment: 'List of systemd services found on host.',
            items: {
              type: 'string',
            },
            type: 'array',
          },
          systemdTimers: {
            $comment: 'List of systemd timers found on host.',
            items: {
              type: 'string',
            },
            type: 'array',
          },
          timezone: {
            $comment: 'Timezone of the host.',
            type: 'string',
          },
        },
        required: [
          'systemdTimers',
          'systemdServices',
          'authorizedSshKeys',
          'publicSshHostKeys',
          'dockerContainers',
          'distribution',
          'kernel',
          'clock',
          'hardware',
          'cloud',
          'timezone',
          'hostname',
          'host',
        ],
        type: 'object',
      },
      type: 'array',
    },
    knownSshKeys: {
      $comment: 'List of known SSH public keys.',
      items: {
        $comment: 'SSH Public Key Information\nSshPublicKey',
        properties: {
          comment: {
            $comment: 'Comment on the public key.',
            type: 'string',
          },
          data: {
            $comment: 'Original information.',
            type: 'string',
          },
          fingerprint: {
            $comment: 'Fingerprint of the public key.',
            type: 'string',
          },
          length: {
            $comment: 'Length of the public key.',
            maximum: 2147483647,
            minimum: -2147483648,
            type: 'number',
          },
          type: {
            $comment: 'Type of the public key.',
            type: 'string',
          },
        },
        required: ['fingerprint', 'comment', 'length', 'type', 'data'],
        type: 'object',
      },
      type: 'array',
    },
    meta: {
      $comment: 'Meta information of the report.\nReport Meta Information\nReportMeta',
      properties: {
        buildHash: {
          $comment: 'Build hash of the application.',
          type: 'string',
        },
        buildTag: {
          $comment: 'Build tag of the application.',
          type: 'string',
        },
        timestamp: {
          $comment: 'Timestamp of the report.\nDate/time in ISO8601 format.',
          type: 'string',
        },
        version: {
          $comment: 'Version of the application.',
          type: 'string',
        },
      },
      required: ['timestamp', 'version'],
      type: 'object',
    },
  },
  required: ['errors', 'meta', 'knownSshKeys', 'hosts'],
  type: 'object',
} as const satisfies JSONSchema;

export type HostPatrolReport = FromSchema<typeof HOSTPATROL_REPORT_SCHEMA>;

export type ArrayElement<ArrayType extends readonly unknown[]> = ArrayType extends readonly (infer ElementType)[]
  ? ElementType
  : never;

export type HostReport = ArrayElement<HostPatrolReport['hosts']>;
export type SshPublicKey = ArrayElement<HostReport['authorizedSshKeys']>;

const AJV = new Ajv();

const HOSTPATROL_REPORT_VALIDATOR = AJV.compile<HostPatrolReport>(HOSTPATROL_REPORT_SCHEMA);

const _LOCAL_STORAGE_KEY_DATA = 'HOSTPATROL_DATA';

export function loadData(): Either<string, Maybe<HostPatrolReport>> {
  const data = localStorage.getItem(_LOCAL_STORAGE_KEY_DATA);

  if (data === null) {
    return Right(Nothing);
  }

  return parseData(data).map(Just);
}

export function parseData(data: string): Either<string, HostPatrolReport> {
  try {
    const parsed = JSON.parse(data);
    const result = HOSTPATROL_REPORT_VALIDATOR(parsed);

    if (!result) {
      console.error(HOSTPATROL_REPORT_VALIDATOR.errors);
      return Left('Invalid Host Patrol report object.');
    }

    return Right(parsed);
  } catch (err) {
    return Left(`Data can not be parsed into JSON. ${err}`);
  }
}

export function saveData(x: HostPatrolReport): void {
  localStorage.setItem(_LOCAL_STORAGE_KEY_DATA, JSON.stringify(x));
}

export function deleteData(): void {
  localStorage.removeItem(_LOCAL_STORAGE_KEY_DATA);
}

export type SshKeysTable = Record<string, SshKeysTableRecord>;

export interface SshKeysTableRecord {
  key: SshPublicKey;
  seenHosts: Set<HostReport>;
  seenComments: Set<string>;
  isKnown: boolean;
  knownComment: string;
}

export function buildSshKeysTable(data: HostPatrolReport): SshKeysTable {
  // Initialize the return value:
  const keys: SshKeysTable = {};

  // Lookup table for known SSH public key comments by their fingerprint:
  const knownComments: Record<string, string> = [
    ...data.knownSshKeys,
    ...data.hosts.reduce((acc, x) => [...acc, ...(x.host.knownSshKeys || [])], [] as typeof data.knownSshKeys),
  ].reduce((acc, x) => ({ ...acc, [x.fingerprint]: x.comment || '<NO-COMMENT>' }), {});

  // Iterate over all SSH public keys for all hosts and populate our registry:
  for (const host of data.hosts) {
    for (const key of host.authorizedSshKeys) {
      if (key.fingerprint in keys) {
        keys[key.fingerprint].seenComments.add(key.comment);
        keys[key.fingerprint].seenHosts.add(host);
      } else {
        keys[key.fingerprint] = {
          key: key,
          seenHosts: new Set([host]),
          seenComments: new Set([key.comment]),
          isKnown: key.fingerprint in knownComments,
          knownComment: knownComments[key.fingerprint],
        };
      }
    }
  }

  // Done, return with the lookup table:
  return keys;
}
