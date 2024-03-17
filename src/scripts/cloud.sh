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

# Prints a key/value pair in SHELL variable format. Value is printed
# within double-quotes, and double-quotes in the variable are escaped.
_print_var() {
    printf '%s="%s"\n' "${1}" "$(echo "${2}" | sed 's/"/\\"/g')"
}

##########
# CHECKS #
##########

_check_command curl

#############
# PROCEDURE #
#############

if curl --silent --fail --max-time 3 http://169.254.169.254/latest/meta-data/ami-id >/dev/null; then
    _print_var "LHP_CLOUD_NAME" "AWS"
    _print_var "LHP_CLOUD_ID" "$(curl -s http://169.254.169.254/latest/meta-data/instance-id)"
    _print_var "LHP_CLOUD_TYPE" "$(curl -s http://169.254.169.254/latest/meta-data/instance-type)"
    _print_var "LHP_CLOUD_REGION" "$(curl -s http://169.254.169.254/latest/meta-data/placement/region)"
    _print_var "LHP_CLOUD_AVAILABILITY_ZONE" "$(curl -s http://169.254.169.254/latest/meta-data/placement/availability-zone)"
    _print_var "LHP_CLOUD_LOCAL_HOSTNAME" "$(curl -s http://169.254.169.254/latest/meta-data/local-hostname)"
    _print_var "LHP_CLOUD_LOCAL_ADDRESS" "$(curl -s http://169.254.169.254/latest/meta-data/local-ipv4)"
    _print_var "LHP_CLOUD_PUBLIC_HOSTNAME" "$(curl -s http://169.254.169.254/latest/meta-data/public-hostname)"
    _print_var "LHP_CLOUD_PUBLIC_ADDRESS" "$(curl -s http://169.254.169.254/latest/meta-data/public-ipv4)"
elif curl --silent --fail --max-time 3 http://169.254.169.254/metadata/v1/ >/dev/null; then
    _print_var "LHP_CLOUD_NAME" "DO"
    _print_var "LHP_CLOUD_ID" "$(curl -s http://169.254.169.254/metadata/v1/id)"
    _print_var "LHP_CLOUD_REGION" "$(curl -s http://169.254.169.254/metadata/v1/region)"
    _print_var "LHP_CLOUD_LOCAL_ADDRESS" "$(curl -s http://169.254.169.254/metadata/v1/interfaces/private/0/ipv4/address)"
    _print_var "LHP_CLOUD_PUBLIC_ADDRESS" "$(curl -s http://169.254.169.254/metadata/v1/interfaces/public/0/ipv4/address)"
    _print_var "LHP_CLOUD_RESERVED_ADDRESS" "$(curl -s http://169.254.169.254/metadata/v1/reserved_ip/ipv4/ip_address)"
elif curl --silent --fail --max-time 3 http://169.254.169.254/latest/meta-data/network-config >/dev/null; then
    _print_var "LHP_CLOUD_NAME" "HETZNER"
    _print_var "LHP_CLOUD_ID" "$(curl -s http://169.254.169.254/latest/meta-data/instance-id)"
    _print_var "LHP_CLOUD_REGION" "$(curl -s http://169.254.169.254/latest/meta-data/region)"
    _print_var "LHP_CLOUD_AVAILABILITY_ZONE" "$(curl -s http://169.254.169.254/latest/meta-data/availability-zone)"
    _print_var "LHP_CLOUD_PUBLIC_ADDRESS" "$(curl -s http://169.254.169.254/latest/meta-data/public-ipv4)"
else
    _print_var "LHP_CLOUD_NAME" "UNKNOWN"
fi
