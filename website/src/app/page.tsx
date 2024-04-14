import { Card, CardBody, CardHeader, Code } from '@nextui-org/react';
import Link from 'next/link';

export default function Page() {
  return (
    <div className="flex w-full flex-col items-center justify-center">
      <Points />

      <div className="mx-auto my-12 max-w-7xl space-y-8">
        <Card className="w-full space-y-6 p-6 lg:p-8">
          <CardHeader className="font-semibold">Install via Nix</CardHeader>

          <CardBody>
            <Code>nix profile install --file https://github.com/vst/lhp/archive/v&lt;VERSION&gt;.tar.gz app</Code>
          </CardBody>

          <CardHeader className="font-semibold">Install Statically Build Executable (x86_64)</CardHeader>

          <CardBody>
            <Code>
              <pre>
                {`curl -Lo /tmp/hostpatrol https://github.com/vst/hostpatrol/releases/latest/download/hostpatrol-static-linux-x86_64
sudo install /tmp/hostpatrol /usr/local/bin/hostpatrol`}
              </pre>
            </Code>
          </CardBody>
        </Card>

        <Card className="w-full space-y-6 p-6 lg:p-8">
          <CardHeader className="font-semibold">Configuration Example</CardHeader>

          <CardBody>
            <Code>
              <pre>
                {`## List of known SSH public keys for all hosts.
knownSshKeys:
  - gh:some-github-user
  - ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKq9bpy0IIfDnlgaTCQk0YhKyKFqInRjoqeIPlBuiFwS test-key-admin

## List of hosts to patrol
hosts:
  - ## Name of the host (required)
    name: somehost
    ## SSH configuration (optional)
    ssh:
      ## SSH destination (required)
      destination: root@10.10.10.10
      ## SSH options (optional)
      options: ["-i", "/keys/hebele.pri"]
    ## External identifier of the host (optional)
    id: 20b88669-743f-4ae5-9823-5aacc2df7086
    ## External URL for the host (optional)
    url: https://internal.documentation/hosts/somehost
    ## List of tags for the host (optional)
    tags:
      - oranges
      - strawberries
    ## Arbitrary JSON data for the host (optional)
    data:
      owner: Client-1
      cost: 50
    ## List of known SSH public keys for the host (optional)
    knownSshKeys:
      - gh:another-github-user
      - ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGmlBxUagOqtWcW6B77TUL8li85ZNYx0tphd3TSx4SEB test-key-tenant
  - name: otherhost
    url: https://internal.documentation/hosts/otherhost
    tags:
      - apples
      - strawberries`}
              </pre>
            </Code>
          </CardBody>
        </Card>

        <Card className="w-full space-y-6 p-6 lg:p-8">
          <CardHeader className="font-semibold">Usage</CardHeader>

          <CardBody className="space-y-8">
            <p>You can pass hosts via CLI arguments:</p>

            <Code>
              <pre>{`hostpatrol compile --host my-host-1 --host my-host-2 > /tmp/hostpatrol-report.json`}</pre>
            </Code>

            <p>This command connects to hosts sequentially and ignores problematic hosts in the output.</p>

            <p>
              To use parallel mode, use `--parallel` flag. In this case, if any of the hosts cause an error, entire
              operation will fail:
            </p>

            <Code>
              <pre>{`hostpatrol compile --parallel --host my-host-1 --host my-host-2 > /tmp/hostpatrol-report.json`}</pre>
            </Code>

            <p>
              Alternatively, you can use the configuration file which has additional benefit of attaching static
              information to your hosts such as external documentation URL and/or tags, and using SSH configuration
              instead of plain host name.
            </p>

            <Code>
              <pre>{`hostpatrol compile --config config.yaml > /tmp/hostpatrol-report.json`}</pre>
            </Code>

            <p>..., or mix with `--host` option:</p>

            <Code>
              <pre>{`hostpatrol compile --config config.yaml --host a-host --host b-host > /tmp/hostpatrol-report.json`}</pre>
            </Code>

            <p>
              You can process/analyse the JSON output itself or use{' '}
              <Link href="/report" className="font-semibold text-indigo-500">
                Web application
              </Link>{' '}
              to list, tabulate and visualise the information.
            </p>
          </CardBody>
        </Card>
      </div>
    </div>
  );
}

function Points() {
  return (
    <div className="py-24 sm:py-32">
      <div className="mx-auto max-w-7xl px-6 lg:px-8">
        <div className="mx-auto max-w-2xl lg:mx-0">
          <h2 className="text-3xl font-bold tracking-tight text-gray-900 sm:text-4xl">Patrol Your Hosts</h2>

          <p className="mt-6 text-lg leading-8 text-gray-600">
            Host Patrol is a command-line tool for gathering information about your hosts via SSH. It also offers a Web
            application for viewing this information in your browser.
          </p>

          <div className="mt-6 flex   space-x-2">
            <img
              alt="GitHub Release"
              src="https://img.shields.io/github/v/release/vst/hostpatrol?display_name=tag&style=for-the-badge"
            />
            <img
              alt="GitHub Issues or Pull Requests"
              src="https://img.shields.io/github/issues/vst/hostpatrol?style=for-the-badge"
            />
            <img
              alt="GitHub Issues or Pull Requests"
              src="https://img.shields.io/github/issues-pr/vst/hostpatrol?style=for-the-badge"
            />
          </div>
        </div>

        <dl className="mx-auto mt-16 grid max-w-2xl grid-cols-1 gap-x-8 gap-y-16 text-base leading-7 sm:grid-cols-2 lg:mx-0 lg:max-w-none lg:grid-cols-3">
          {points.map((point) => (
            <div key={point.name}>
              <dt className="font-semibold text-gray-900">{point.name}</dt>
              <dd className="mt-1 text-balance text-gray-600">{point.description}</dd>
            </div>
          ))}
        </dl>
      </div>
    </div>
  );
}

const points = [
  {
    name: 'Who is it for?',
    description: (
      <>
        Host Patrol is for people who (1) manage multiple hosts on the cloud or on their own infrastructure, (2) need to
        build a registry of such hosts, but (3) want a simple, hackable and free solution.
      </>
    ),
  },
  {
    name: 'What does it do?',
    description: (
      <>
        It collects information about your hosts through SSH and offers a hosted and privacy-preserving{' '}
        <Link href="/report" className="font-semibold text-indigo-500">
          Web application
        </Link>{' '}
        for conveniently viewing this data in your browser.
      </>
    ),
  },
  {
    name: 'How does it work?',
    description: (
      <>
        You install the Host Patrol command-line tool on your local machine, prepare a configuration file, and run the
        tool with the configuration to generate a JSON file containing information about your hosts.
      </>
    ),
  },
  {
    name: 'How does it collect the information?',
    description: (
      <>
        Assuming that you can SSH to the remote host, Host Patrol issues{' '}
        <a href="https://github.com/vst/hostpatrol/tree/main/src/scripts" className="font-semibold text-indigo-500">
          simple POSIX-shell commands
        </a>{' '}
        to your remote host and compiles the output into a JSON file.
      </>
    ),
  },
  {
    name: 'What is the status of the project?',
    description: (
      <>
        This project is still in its early stages and currently fulfills the author&apos;s requirements. If you are
        interested in this or similar solutions, please consider{' '}
        <a href="https://github.com/vst/hostpatrol" className="font-semibold text-indigo-500">
          starring it on GitHub
        </a>
        .
      </>
    ),
  },
  {
    name: 'How can you contribute?',
    description: (
      <>
        This project contains Haskell, Shell scripts, Nix and Typescript. It is licensed under the{' '}
        <a href="https://opensource.org/license/mit" className="font-semibold text-indigo-500">
          MIT license
        </a>
        . Contributions are welcome through{' '}
        <a href="https://github.com/vst/hostpatrol/issues" className="font-semibold text-indigo-500">
          issues
        </a>{' '}
        or{' '}
        <a href="https://github.com/vst/hostpatrol/pulls" className="font-semibold text-indigo-500">
          pull requests
        </a>{' '}
        on GitHub.
      </>
    ),
  },
];
