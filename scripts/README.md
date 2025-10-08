# Scripts

This directory contains scripts which are sent to and run on the target hosts by
Host Patrol during information retrieval.

These scripts are written in POSIX-compliant `sh` and should work on any
UNIX-like system. They do not require any additional dependencies except for
standard UNIX tools like `awk`, `sed`, `grep`, etc. which are expected to be
present on remote hosts.

Major concerns when writing these scripts are:

1. **Portability**: The scripts should work on a wide range of UNIX-like
   systems, including various Linux distributions, BSD variants, and macOS.
2. **Minimal dependencies**: The scripts should not rely on non-standard tools
   or libraries that may not be available on all target systems.
3. **Efficiency**: The scripts should be efficient in terms of execution time
   and resource usage, especially when run on multiple hosts concurrently.
4. **Error handling**: The scripts should handle errors gracefully and provide
   meaningful error messages to help diagnose issues.
5. **Security**: The scripts should avoid using insecure practices, such as
   executing untrusted code or exposing sensitive information.
6. **Confidentiality**: The scripts should avoid collecting or transmitting
   sensitive information unless absolutely necessary and should comply with
   relevant privacy regulations and organizational policies.
7. **Read-only operations**: The scripts should not modify the state of the
   target systems. They should only read information and report it back to the
   Host Patrol tool.

When adding new scripts or modifying existing ones, please ensure that they
adhere to these principles to maintain the overall quality and reliability of
the Host Patrol tool.

## Testing

Since these scripts are executed on remote hosts over SSH, testing them should
be easy:

```sh
cat clock.sh | ssh -T my-remote-host 'sh -s --'
```

If you want to test all scripts at once, you can use the following command:

```sh
find . -iname "*.sh" | sort | while read -r _path; do
  printf "## %s\n\n" "${_path}"
  cat "${_path}" | ssh -T my-remote-host 'sh -s --'
  echo
done
```
