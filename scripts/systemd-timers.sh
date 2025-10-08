#!/usr/bin/env sh

#############
# PROCEDURE #
#############

## Check if systemctl (therefore systemd) is available:
if ! which "systemctl" >/dev/null; then
    exit 0
fi

## List timers:
systemctl list-unit-files --state enabled --type timer |
    tail -n +2 | head -n -2 | awk '{print $1}'
