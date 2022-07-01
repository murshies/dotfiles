#!/bin/sh

cleanup() {
    echo "Cleaning up virtualenv $venv_name"
    rm -r $venv_name
    trap '' EXIT INT TERM
}
trap cleanup EXIT INT TERM

venv_name=$(mktemp -d)
echo "Creating virtualenv in $venv_name"
python3 -m venv $venv_name
. $venv_name/bin/activate
pip install --upgrade pip
pip install -r requirements.txt
