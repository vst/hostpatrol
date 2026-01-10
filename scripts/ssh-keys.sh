#!/usr/bin/env sh
# shellcheck disable=SC2046

###################
# SHELL BEHAVIOUR #
###################

# Stop on errors:
set -e

#############
# PROCEDURE #
#############

find \
  /etc/ssh/authorized_keys.d/* \
  $(cut -f6 -d ':' /etc/passwd | sort | uniq | xargs -I{} echo "{}/.ssh/authorized_keys") \
  $(cut -f6 -d ':' /etc/passwd | sort | uniq | xargs -I{} echo "{}/.ssh/authorized_keys2") \
  2>/dev/null |
  sort -u |
  xargs -I{} cat {} |
  xargs -I{} echo {} |
  grep -vE "^#" |
  sort -u |
  tr -s ' '
