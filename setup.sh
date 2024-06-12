#!/bin/bash

cleanup() {
    if [ ! -z "$CLEANUP_SETUP_VENV" ]; then
        echo "Cleaning up virtualenv $venv_name"
        rm -r $venv_name
    fi
    if [ ! -z "$switch_to_permissive_sudo" ]; then
        ./user-sudo.sh
    fi
    trap '' EXIT INT TERM
}
trap cleanup EXIT INT TERM

venv_name=/tmp/setup-venv-$(printf $(sha256sum requirements.txt))
if [ ! -d "$venv_name" ]; then
    echo "Creating virtualenv in $venv_name"
    python3 -m venv $venv_name
    . $venv_name/bin/activate
    pip install --upgrade pip
    pip install -r requirements.txt
else
    echo "venv $venv_name already exists, reusing it"
    . $venv_name/bin/activate
fi

if ! grep -F 'NOPASSWD: ALL' /etc/sudoers.d/$(whoami) > /dev/null 2>&1 && [[ "$@" != *"-l"* ]]; then
    ./user-sudo-all.sh
    switch_to_permissive_sudo=y
fi

export PYTHONPATH="$(dirname $(realpath "$0"))"
echo "PYTHONPATH is $PYTHONPATH"
./setup.py "$@"
