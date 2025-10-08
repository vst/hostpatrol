#!/usr/bin/env sh

###################
# SHELL BEHAVIOUR #
###################

# Stop on errors:
set -e

###############
# DEFINITIONS #
###############

# Prints a key/value pair in SHELL variable format. Value is printed
# within double-quotes, and double-quotes in the variable are escaped.
_print_var() {
    printf '%s="%s"\n' "${1}" "$(echo "${2}" | sed 's/"/\\"/g')"
}

##########
# CHECKS #
##########

## Check if timedatectl is available:
if ! command -v timedatectl >/dev/null; then
    _print_var "HOSTPATROL_CLOCK_NTP" "timedatectl not available"
    _print_var "HOSTPATROL_CLOCK_NTP_SYNCHRONIZED" "timedatectl not available"
    exit 0
fi

## Check if we have a recent enough version of timedatectl:
if ! timedatectl show 2>/dev/null; then
    _print_var "HOSTPATROL_CLOCK_NTP" "timedatectl failed (too old?)"
    _print_var "HOSTPATROL_CLOCK_NTP_SYNCHRONIZED" "timedatectl failed (too old?)"
    exit 0
fi

#############
# PROCEDURE #
#############

_print_var "HOSTPATROL_CLOCK_NTP" "$(timedatectl show --property=NTP --value)"
_print_var "HOSTPATROL_CLOCK_NTP_SYNCHRONIZED" "$(timedatectl show --property=NTPSynchronized --value)"
