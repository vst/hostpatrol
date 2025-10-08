#!/usr/bin/env sh

###################
# SHELL BEHAVIOUR #
###################

# Stop on errors:
set -e

###############
# DEFINITIONS #
###############

# Checks if a given command exist on the host (is on $PATH), and exits
# this script with exit code 80 if not.
_check_command() {
    if ! which "${1}" >/dev/null; then
        echo >&2 "Command not found: ${1}"
        exit 80
    fi
}

# Checks if a given file exist on the host, and exits
# this script with exit code 81 if not.
_check_file() {
    if [ ! -f "${1}" ]; then
        echo >&2 "File not found: ${1}"
        exit 81
    fi
}

# Prints a key/value pair in SHELL variable format. Value is printed
# within double-quotes, and double-quotes in the variable are escaped.
_print_var() {
    printf '%s="%s"\n' "${1}" "$(echo "${2}" | sed 's/"/\\"/g')"
}

# Prints the name of the given command if it is on the PATH, returns
# with 1 otherwise.
_which_command() {
    which "${1}" >/dev/null 2>&1 && echo "${1}" || return 1
}

##########
# CHECKS #
##########

_check_command uname
_check_command nproc
_check_file /etc/os-release

#############
# PROCEDURE #
#############

_print_var "HOSTPATROL_GENERAL_HOSTNAME" "$(hostname)"
_print_var "HOSTPATROL_GENERAL_TIMEZONE" "$(timedatectl | grep "Time zone" | cut -f 2- -d ":" | xargs)"
_print_var "HOSTPATROL_HW_CPU" "$(nproc)"
_print_var "HOSTPATROL_HW_RAM" "$(grep -oP 'MemTotal:\s+\K\d+' /proc/meminfo)"
_print_var "HOSTPATROL_HW_DISK" "$(df -k --output=size / | tail -n +2 | grep -o '[[:digit:]]*')"
_print_var "HOSTPATROL_KERNEL_NAME" "$(uname -s)"
_print_var "HOSTPATROL_KERNEL_NODE" "$(uname -n)"
_print_var "HOSTPATROL_KERNEL_RELEASE" "$(uname -r)"
_print_var "HOSTPATROL_KERNEL_VERSION" "$(uname -v)"
_print_var "HOSTPATROL_KERNEL_MACHINE" "$(uname -m)"
_print_var "HOSTPATROL_KERNEL_OS" "$(uname -o)"
_print_var "HOSTPATROL_APP_DOCKER" "$(_which_command "docker" || _which_command "podman" || echo "")"
_print_var "HOSTPATROL_APP_SCREEN" "$(_which_command "screen" || echo "")"
_print_var "HOSTPATROL_APP_TMUX" "$(_which_command "tmux" || echo "")"
sed "s/^/HOSTPATROL_DISTRO_/g" /etc/os-release
