<h1 align="center">
    <div>
        <img alt="Host Patrol Icon" src="https://media.hostpatrol.io/site/meta/icon-dark.svg" width="180" />
    </div>
    <div>HOST PATROL</div>
    <div>
      <sub>Lazy Hacker's Linux Host Patrol</sub>
    </div>
    <div>
        <sub>
          <a href="https://www.hostpatrol.io">Website</a> •
          <a href="https://console.hostpatrol.io">Console</a>
        <sub>
    </div>
    <div>
        <img alt="GitHub Release" src="https://img.shields.io/github/v/release/vst/hostpatrol?display_name=tag&style=flat-square" />
        <img alt="GitHub Issues" src="https://img.shields.io/github/issues/vst/hostpatrol?style=flat-square" />
        <img alt="Pull Requests" src="https://img.shields.io/github/issues-pr/vst/hostpatrol?style=flat-square" />
        <img alt="CI Status" src="https://img.shields.io/github/actions/workflow/status/vst/hostpatrol/check.yaml?style=flat-square" />
    </div>
</h1>

![Host Patrol Web UI Screenshot](https://github.com/vst/hostpatrol/assets/374793/416e1135-fe9a-4998-8acc-de07dd62c88b)

Host Patrol (`hostpatrol`) is a command-line application to retrieve information
from remote hosts. The information is retrieved over SSH by executing small
[scripts]. Then, the information is compiled into JSON to be further analysed or
visualised on [Host Patrol Console Application][console].

The report contains following information for each host:

1. **Hardware resources:** (v)CPU count, total RAM, root filesystem disk size
1. **Kernel information**
1. **Linux distribution information** as in [os-release]
1. List of all **Docker containers** (in all states)
1. List of **systemd services**
1. List of **systemd timers**
1. List of **authorised SSH public keys** found on the host
1. **Cloud information:** (if any)
   - Cloud provider name
   - Host instance identifier
   - Host type
   - Region
   - Availability zone
   - Local hostname
   - Local address
   - Remote hostname
   - Remote address
   - Reserved address

## Installation

Currently, the easiest way to install the command-line application is via Nix:

```sh
nix profile install --file https://github.com/vst/hostpatrol/archive/v<VERSION>.tar.gz
```

Alternatively, you can use the statically compiled binary distributed along with
each release (currently Linux x86_64 only).

## Usage

`hostpatrol` assumes that you are able to connect to remote hosts via SSH:

```sh
ssh my-host
```

Indeed, `hostpatrol` uses `ssh` under the hood. Therefore, if you can `ssh`, you
can `hostpatrol`!

> [!NOTE]
>
> If you are using SSH public key authentication to connect to host and your SSH
> private key is password-protected, use SSH-agent and unlock your private key
> first.

You can pass hosts via CLI arguments:

```sh
hostpatrol compile --host my-host-1 --host my-host-2 > /tmp/hostpatrol-report.json
```

This command connects to hosts in parallel and ignores all failed hosts by
reporting errors in the output.

> If you want to change the number of maximum number of threads to use for
> concurrent patrol tasks, do so with `--threads` option that defaults to `4`
> otherwise:
>
> ```sh
> hostpatrol compile --threads 10 --host my-host-1 --host my-host-2 ... > /tmp/hostpatrol-report.json
> ```

Alternatively, you can use a configuration file which has additional benefit of
attaching static information to your hosts such as external documentation URL
and/or tags, and using SSH configuration instead of plain host name. The
configuration file looks like as follows:

<!-- prettier-ignore-start -->
```yaml
## config.yaml
## List of known SSH public keys for all hosts.
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
      - strawberries
```
<!-- prettier-ignore-end -->

Then, you can use this configuration file instead of specifying hosts
individually on the command-line:

```sh
hostpatrol compile --config config.yaml > /tmp/hostpatrol-report.json
```

..., or mix with `--host` option:

```sh
hostpatrol compile --config config.yaml --host a-host --host b-host > /tmp/hostpatrol-report.json
```

Users can process/analyse the JSON output themselves or use [Host Patrol Console
Application][console] to list, tabulate and visualise the information.

> [!NOTE]
>
> The [console] application stores the report locally in the Web browser using
> local storage. It is not sent to any third party service. You can study the
> [source code of the console application][console-source] that is automatically
> published to GitHub Pages.

## Development

The codebase comes with a Nix shell powered by Nix Flakes:

```sh
nix develop
```

Alternatively, you can use `direnv`:

```sh
cp .envrc.tmpl .envrc
direnv allow
```

Run following command to create the git-ignored `.cabal` file, perform checks,
tests and build the project:

```sh
cabal dev-test-build
```

Subsequently, you may pass `-c` option to this command to clean up the build
artifacts first:

```sh
cabal dev-test-build -c
```

## License

Copyright &copy; 2024-2025 Vehbi Sinan Tunalioglu. This work is licensed under
[MIT License].

<!-- REFERENCES -->

[Issue 32]: https://github.com/vst/hostpatrol/issues/32
[website]: https://www.hostpatrol.io
[console]: https://console.hostpatrol.io
[console-source]: https://github.com/vst/hostpatrol-console
[os-release]:
  https://www.freedesktop.org/software/systemd/man/latest/os-release.html
[scripts]: ./scripts
[MIT License]: https://opensource.org/license/mit
