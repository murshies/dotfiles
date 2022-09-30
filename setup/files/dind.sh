#!/bin/bash

if [ ! -S /var/run/docker.sock ]; then
    # No docker socket file, so nothing to do
    exit 0
fi

if [ $(stat -c %G /var/run/docker.sock) == "UNKNOWN" ]; then
    docker_gid=$(stat -c %g /var/run/docker.sock)
    sudo groupadd -g $docker_gid dind
fi

docker_group=$(stat -c %G /var/run/docker.sock)
sudo usermod -a -G $docker_group $(whoami)
