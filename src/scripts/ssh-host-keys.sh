#!/usr/bin/env sh

###################
# SHELL BEHAVIOUR #
###################

# Stop on errors:
set -e

#############
# PROCEDURE #
#############

find "/etc/ssh" -iname 'ssh_host_*.pub' -exec cat {} \;
