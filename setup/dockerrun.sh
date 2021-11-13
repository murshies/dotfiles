#!/bin/bash

container_name="$1"
ssh_port="$2"

if [ -z "$ssh_port" ]; then
    ssh_port=2222
fi

if [ -z "$container_name" ] ; then
    echo "Usage: $0 <container_name> [<ssh_port>]"
    exit 1
fi

docker run -p $ssh_port:22 -v /var/run/docker.sock:/var/run/docker.sock -v $HOME:/root/host --name "$container_name" -d murshies/devenv:latest bash /home/user/bin/server-mode.sh
