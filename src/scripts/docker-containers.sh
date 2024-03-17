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

# Prints the name of the given command if it is on the PATH, returns
# with 1 otherwise.
_which_command() {
    which "${1}" >/dev/null 2>&1 && echo "${1}" || return 1
}

# Returns the Docker command:
_which_docker() {
    _which_command "docker" || _which_command "podman" || {
        echo >&2 "No docker/podman command found."
        exit 80
    }
}

##########
# CHECKS #
##########

_docker="$(_which_docker)"

#############
# PROCEDURE #
#############

"${_docker}" ps --all --quiet | xargs "${_docker}" inspect
