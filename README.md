<h1 align="center">
    <div>
        <img alt="logo" src="https://github.com/vst/lhp/assets/374793/cad6706b-0384-4a71-9ca5-21ade9ce3d1e" />
    </div>
    <div>Host Patrol</div>
    <sub>Lazy Hacker's Linux Host Patrol</sub>
    <p></p>
    <div>
        <img alt="GitHub Release" src="https://img.shields.io/github/v/release/vst/lhp?display_name=tag&style=for-the-badge">
        <img alt="GitHub Issues or Pull Requests" src="https://img.shields.io/github/issues/vst/lhp?style=for-the-badge">
        <img alt="GitHub Issues or Pull Requests" src="https://img.shields.io/github/issues-pr/vst/lhp?style=for-the-badge">
    </div>
</h1>

Host Patrol (`lhp`) is a command-line application to retrieve
information from remote hosts. The information is retrieved over SSH
by executing small [scripts]. Then, the information is compiled into
JSON to be further analysed or visualised on its [Website].

The report contains following information for each host:

1. **Hardware resources:** (v)CPU count, total RAM, root filesystem disk size
1. **Kernel information**
1. **Linux distribution information** as in [os-release]
1. List of all **Docker containers** (in all states)
1. List of **systemd services**
1. List of **systemd timers**
1. List of **authorised SSH public keys**  found on the host
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

> [!NOTE]
>
> A statically compiled executable is planed. See [Issue 32].

Currently, the easiest way to install the command-line application is
via Nix:

```sh
nix profile install --file https://github.com/vst/lhp/archive/v<VERSION>.tar.gz app
```

## Usage

`lhp` assumes that you are able to connect to remote hosts via SSH:

```sh
ssh my-host
```

Indeed, `lhp` uses `ssh` under the hood. Therefore, if you can `ssh`,
you can `lhp`!

> [!NOTE]
>
> If you are using SSH public key authentication to connect to host
> and your SSH private key is password-protected, use SSH-agent and
> unlock your private key first.

You can pass hosts via CLI arguments:

```sh
lhp compile --host my-host-1 --host my-host-2 > /tmp/lhp-report.json
```

This command connects to hosts sequentially and ignores problematic
hosts in the output.

To use parallel mode, use `--parallel` flag. In this case, if any of
the hosts cause an error, entire operation will fail:

```sh
lhp compile --parallel --host my-host-1 --host my-host-2 > /tmp/lhp-report.json
```

Alternatively, you can use a configuration file which has additional
benefit of attaching static information to your hosts such as external
documentation URL and/or tags, and using SSH configuration instead of
plain host name. The configuration file looks like as follows:

```yaml
## config.yaml
## List of known SSH public keys to be added to the report.
##
## These can be then used by external programs of lhp Web UI to
## highlight if a host has an unknown authorized SSH public key.
knownSshKeys:
  - ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKq9bpy0IIfDnlgaTCQk0YhKyKFqInRjoqeIPlBuiFwS testing

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
    ## External URL for the host (optional)
    url: https://internal.documentation/hosts/somehost
    ## List of tags for the host (optional)
    tags:
      - oranges
      - strawberries
  - name: otherhost
    url: https://internal.documentation/hosts/otherhost
    tags:
      - apples
      - strawberries
```

Then, you can use this configuration file instead of specifying hosts
individually on the command-line:

```sh
lhp compile --config config.yaml > /tmp/lhp-report.json
```

..., or mix with `--host` option:

```sh
lhp compile --config config.yaml --host a-host --host b-host > /tmp/lhp-report.json
```

Users can process/analyse the JSON output themselves or use [Website]
to list, tabulate and visualise the information.

> [!NOTE]
>
> The [Website] stores the report locally in the Web browser using
> local storage. It is not sent to any third party service. You can
> study the [Website source-code] that is automatically published to
> GitHub Pages.

## Development

The codebase comes with a Nix shell. You can use `direnv` for convenience:

```sh
direnv allow
```

Big, long format, lint, build and test command for the impatient:

```sh
hpack &&
    direnv reload &&
    fourmolu -i app/ src/ test/ &&
    prettier --write . &&
    find . -iname "*.nix" -not -path "*/nix/sources.nix" -and -not -path "*/website/node_modules/*.nix" -print0 | xargs --null nixpkgs-fmt &&
    hlint app/ src/ test/ &&
    cabal build -O0 &&
    cabal run -O0 lhp -- --version &&
    cabal v1-test &&
    cabal haddock -O0
```

For testing and building:

```sh
dev-test-build
```

## License

Copyright &copy; 2024 Vehbi Sinan Tunalioglu. This work is licensed
under [MIT License].

<!-- REFERENCES -->

[Issue 32]: https://github.com/vst/lhp/issues/32
[Website]: https://thenegation.com/lhp
[os-release]: https://www.freedesktop.org/software/systemd/man/latest/os-release.html
[scripts]: ./src/scripts
[Website source-code]: ./website
[MIT License]: https://opensource.org/license/mit
