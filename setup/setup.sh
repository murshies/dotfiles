#!/bin/sh

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
export PYTHONPATH=$(dirname $(realpath "$0"))
echo "PYTHONPATH is $PYTHONPATH"
./setup.py "$@"
