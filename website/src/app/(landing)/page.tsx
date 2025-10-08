import { Logo } from '@/components/header';
import Link from 'next/link';

export default function Page() {
  return (
    <div className="flex w-full flex-col items-center justify-center">
      <Hero />
      <LearnMore />
    </div>
  );
}

function Hero() {
  return (
    <div className="relative isolate w-full overflow-hidden bg-white">
      <svg
        className="absolute inset-0 -z-10 h-full w-full stroke-gray-200 [mask-image:radial-gradient(100%_100%_at_top_right,white,transparent)]"
        aria-hidden="true"
      >
        <defs>
          <pattern
            id="0787a7c5-978c-4f66-83c7-11c213f99cb7"
            width={200}
            height={200}
            x="50%"
            y={-1}
            patternUnits="userSpaceOnUse"
          >
            <path d="M.5 200V.5H200" fill="none" />
          </pattern>
        </defs>
        <rect width="100%" height="100%" strokeWidth={0} fill="url(#0787a7c5-978c-4f66-83c7-11c213f99cb7)" />
      </svg>

      <div className="mx-auto max-w-7xl px-6 pb-24 pt-10 sm:pb-32 lg:flex lg:px-8 lg:py-40">
        <div className="mx-auto max-w-2xl lg:mx-0 lg:max-w-xl lg:flex-shrink-0 lg:pt-8">
          <div className="hidden lg:block">
            <Logo />
          </div>

          <div className="mt-6 flex space-x-2">
            <a href="https://github.com/vst/hostpatrol/releases/latest">
              <img
                alt="GitHub Release"
                src="https://img.shields.io/github/v/release/vst/hostpatrol?display_name=tag&style=for-the-badge"
              />
            </a>

            <a href="https://github.com/vst/hostpatrol/issues">
              <img alt="GitHub Issues" src="https://img.shields.io/github/issues/vst/hostpatrol?style=for-the-badge" />
            </a>

            <a href="https://github.com/vst/hostpatrol/pulls">
              <img
                alt="GitHub Pull Requests"
                src="https://img.shields.io/github/issues-pr/vst/hostpatrol?style=for-the-badge"
              />
            </a>

            <a href="https://github.com/vst/hostpatrol/actions/workflows/check.yaml">
              <img
                alt="GitHub Actions Workflow Status"
                src="https://img.shields.io/github/actions/workflow/status/vst/hostpatrol/check.yaml?style=for-the-badge"
              />
            </a>
          </div>

          <h1 className="mt-10 text-3xl font-bold tracking-tight text-gray-900 sm:text-5xl">
            <span className="block">Patrol Your Hosts</span>
          </h1>

          <p className="mt-6 text-balance text-lg leading-8 text-gray-600">
            Host Patrol is a command-line tool and a Web interface for collecting and consolidating information about
            your hosts via SSH.
          </p>

          <div className="mt-10 flex items-center gap-x-6">
            <a
              href="/quickstart"
              className="rounded-md bg-indigo-600 px-3.5 py-2.5 text-sm font-semibold text-white shadow-sm hover:bg-indigo-500 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-600"
            >
              Quick Start
            </a>

            <a href="#learn-more" className="text-sm font-semibold leading-6 text-gray-900">
              Learn more <span aria-hidden="true">→</span>
            </a>
          </div>
        </div>

        <div className="mx-auto mt-16 flex max-w-2xl sm:mt-24 lg:ml-10 lg:mr-0 lg:mt-0 lg:max-w-none lg:flex-none xl:ml-32">
          <div className="max-w-3xl flex-none sm:max-w-5xl lg:max-w-none">
            <div className="-m-2 rounded-xl bg-gray-900/5 p-2 ring-1 ring-inset ring-gray-900/10 lg:-m-4 lg:rounded-2xl lg:p-4">
              <img
                src="https://github.com/vst/hostpatrol/assets/374793/416e1135-fe9a-4998-8acc-de07dd62c88b"
                alt="Web UI Screenshot (with Mock Data)"
                width={1634}
                height={801}
                className="w-[76rem] rounded-md shadow-2xl ring-1 ring-gray-900/10"
              />
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}

function LearnMore() {
  return (
    <div id="learn-more" className="space-y-16 py-24 sm:py-32">
      <div className="mx-auto max-w-7xl px-6 lg:px-8">
        <dl className="mx-auto mt-16 grid max-w-2xl grid-cols-1 gap-x-8 gap-y-16 text-base leading-7 sm:grid-cols-2 lg:mx-0 lg:max-w-none lg:grid-cols-3">
          {POINTS.map((point) => (
            <div key={point.name}>
              <dt className="font-semibold text-gray-900">{point.name}</dt>
              <dd className="mt-1 text-balance text-gray-600">{point.description}</dd>
            </div>
          ))}
        </dl>
      </div>

      <div className="mx-auto flex w-full max-w-2xl justify-center">
        <Link
          href="/quickstart"
          className="rounded-md bg-indigo-600 px-3.5 py-2.5 text-sm font-semibold text-white shadow-sm hover:bg-indigo-500 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-600"
        >
          Proceed to Quick Start
        </Link>
      </div>
    </div>
  );
}

const POINTS = [
  {
    name: 'Who is it for?',
    description: (
      <>
        Host Patrol is for sysadmins and devops who manage hosts on the cloud and/or their own infrastructure to build a
        registry of such hosts, keep track of server access and services, and yet, need a simple and free solution.
      </>
    ),
  },
  {
    name: 'What does it do?',
    description: (
      <>
        Host Patrol collects information about your hosts through SSH and compiles the report into a JSON file. It also
        offers a hosted and privacy-preserving{' '}
        <Link href="/report" className="font-semibold text-indigo-500">
          Web-based tool
        </Link>{' '}
        for conveniently rendering this report in your browser.
      </>
    ),
  },
  {
    name: 'How does it work?',
    description: (
      <>
        You install the Host Patrol command-line tool on your local machine, (optionally) prepare a configuration file,
        and run the tool to generate the report as a JSON file containing information about your hosts.
      </>
    ),
  },
  {
    name: 'How does it collect the information?',
    description: (
      <>
        Host Patrol issues{' '}
        <a href="https://github.com/vst/hostpatrol/tree/main/scripts" className="font-semibold text-indigo-500">
          simple and safe POSIX-shell commands
        </a>{' '}
        to your remote hosts and compiles the output into a JSON file. To achieve this, it uses your vanilla `ssh`
        program.
      </>
    ),
  },
  {
    name: 'What is the status of the project?',
    description: (
      <>
        This project is still in its early stages. Consider{' '}
        <a href="https://github.com/vst/hostpatrol" className="font-semibold text-indigo-500">
          starring it
        </a>{' '}
        and{' '}
        <a href="https://github.com/vst/hostpatrol/issues" className="font-semibold text-indigo-500">
          creating issues
        </a>{' '}
        on GitHub to help support its development. It can get only better with your feedback.
      </>
    ),
  },
  {
    name: 'How can you contribute?',
    description: (
      <>
        This project is built using Haskell, Shell scripts, Nix and Typescript. It is licensed under the{' '}
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
