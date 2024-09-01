#!/bin/bash

cleanup() {
    if [ ! -z "$switch_to_permissive_sudo" ]; then
        ./user-sudo.sh
    fi
    trap '' EXIT INT TERM
}
trap cleanup EXIT INT TERM

if ! grep -F 'NOPASSWD: ALL' /etc/sudoers.d/$(whoami) > /dev/null 2>&1 && [[ "$@" != *"-l"* ]]; then
    ./user-sudo-all.sh
    switch_to_permissive_sudo=y
fi

export PYTHONPATH="$(dirname $(realpath "$0"))"
export NEEDRESTART_MODE=a
export DEBIAN_FRONTEND=noninteractive
echo "PYTHONPATH is $PYTHONPATH"
$PYTHONPATH/setup.py "$@"
