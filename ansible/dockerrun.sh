#!/bin/bash

container_name="$1"
vnc_passwd="$2"

if [ -z "$container_name" ] || [ -z "$vnc_passwd" ]; then
    echo "Usage: $0 <container_name> <vnc_passwd>"
    exit 1
fi

docker run -p 5901:5901 -e VNCPASSWD="$vnc_passwd" -v /var/run/docker.sock:/var/run/docker.sock -v $HOME:/root/host --name "$container_name" -d devenv
