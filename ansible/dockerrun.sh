#!/bin/bash

container_name="$1"
vnc_passwd="$2"
vnc_port="$3"

if [ -z "$vnc_port" ]; then
    vnc_port=5901
fi

if [ -z "$container_name" ] || [ -z "$vnc_passwd" ]; then
    echo "Usage: $0 <container_name> <vnc_passwd> [<vnc_port>]"
    exit 1
fi

docker run -p $vnc_port:5901 -e VNCPASSWD="$vnc_passwd" -v /var/run/docker.sock:/var/run/docker.sock -v $HOME:/root/host --name "$container_name" -d murshies/devenv:latest sleep infinity
