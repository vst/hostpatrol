#!/usr/bin/env sh

#############
# PROCEDURE #
#############

## Check if systemctl (therefore systemd) is available:
if ! which "systemctl" >/dev/null; then
    exit 0
fi

## List services:
systemctl list-unit-files --state enabled --type service |
    tail -n +2 | head -n -2 | awk '{print $1}'
