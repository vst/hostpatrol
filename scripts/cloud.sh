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

# Reads AWS instance metadata using the IMDSv2 session token. The token is
# kept only in this process and expires after a short while.
_aws_metadata() {
  curl \
    --silent \
    --fail \
    --max-time 2 \
    --header "x-aws-ec2-metadata-token: ${_aws_imds_token}" \
    "http://169.254.169.254/latest/meta-data/${1}"
}

# Attempts to retrieve an AWS IMDSv2 session token. Fails silently and returns
# an empty string if the token cannot be retrieved.
_aws_get_token() {
  curl \
    --silent \
    --fail \
    --max-time 2 \
    --request PUT \
    --header 'X-aws-ec2-metadata-token-ttl-seconds: 60' \
    http://169.254.169.254/latest/api/token 2>/dev/null || true
}

# Attempts to retrieve the AWS instance identity document.
_aws_get_instance_document() {
  curl \
    --silent \
    --fail \
    --max-time 2 \
    --header "x-aws-ec2-metadata-token: ${_aws_imds_token}" \
    http://169.254.169.254/latest/dynamic/instance-identity/document
}

##########
# CHECKS #
##########

_check_command curl

#############
# PROCEDURE #
#############

# Attempt to retrieve an AWS IMDSv2 session token. It may fail if the host is
# not running on AWS, in which case the variable will be empty.
_aws_imds_token="$(_aws_get_token)"

# Attempt to get cloud provider information. The order of the checks is important, as some
# cloud providers may return a 200 OK response for the metadata endpoint, but not provide
# the expected metadata.
if [ -n "${_aws_imds_token}" ] && _aws_get_instance_document >/dev/null; then
  _print_var "HOSTPATROL_CLOUD_NAME" "AWS"
  _print_var "HOSTPATROL_CLOUD_ID" "$(_aws_metadata instance-id)"
  _print_var "HOSTPATROL_CLOUD_TYPE" "$(_aws_metadata instance-type)"
  _print_var "HOSTPATROL_CLOUD_REGION" "$(_aws_metadata placement/region)"
  _print_var "HOSTPATROL_CLOUD_AVAILABILITY_ZONE" "$(_aws_metadata placement/availability-zone)"
  _print_var "HOSTPATROL_CLOUD_LOCAL_HOSTNAME" "$(_aws_metadata local-hostname)"
  _print_var "HOSTPATROL_CLOUD_LOCAL_ADDRESS" "$(_aws_metadata local-ipv4)"
  _print_var "HOSTPATROL_CLOUD_PUBLIC_HOSTNAME" "$(_aws_metadata public-hostname)"
  _print_var "HOSTPATROL_CLOUD_PUBLIC_ADDRESS" "$(_aws_metadata public-ipv4)"
elif curl --silent --fail --max-time 2 http://169.254.169.254/metadata/v1/ >/dev/null; then
  _print_var "HOSTPATROL_CLOUD_NAME" "DO"
  _print_var "HOSTPATROL_CLOUD_ID" "$(curl -s http://169.254.169.254/metadata/v1/id)"
  _print_var "HOSTPATROL_CLOUD_REGION" "$(curl -s http://169.254.169.254/metadata/v1/region)"
  _print_var "HOSTPATROL_CLOUD_LOCAL_ADDRESS" "$(curl -s http://169.254.169.254/metadata/v1/interfaces/private/0/ipv4/address)"
  _print_var "HOSTPATROL_CLOUD_PUBLIC_ADDRESS" "$(curl -s http://169.254.169.254/metadata/v1/interfaces/public/0/ipv4/address)"
  _print_var "HOSTPATROL_CLOUD_RESERVED_ADDRESS" "$(curl -s http://169.254.169.254/metadata/v1/reserved_ip/ipv4/ip_address)"
elif curl --silent --fail --max-time 2 http://169.254.169.254/latest/meta-data/network-config >/dev/null; then
  _print_var "HOSTPATROL_CLOUD_NAME" "HETZNER"
  _print_var "HOSTPATROL_CLOUD_ID" "$(curl -s http://169.254.169.254/latest/meta-data/instance-id)"
  _print_var "HOSTPATROL_CLOUD_REGION" "$(curl -s http://169.254.169.254/latest/meta-data/region)"
  _print_var "HOSTPATROL_CLOUD_AVAILABILITY_ZONE" "$(curl -s http://169.254.169.254/latest/meta-data/availability-zone)"
  _print_var "HOSTPATROL_CLOUD_PUBLIC_ADDRESS" "$(curl -s http://169.254.169.254/latest/meta-data/public-ipv4)"
else
  _print_var "HOSTPATROL_CLOUD_NAME" "UNKNOWN"
fi
