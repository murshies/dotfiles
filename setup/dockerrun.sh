#!/bin/bash

docker run $@ -it  -e LOCAL_UID=$(id -u) -e LOCAL_GID=$(id -g) -v /var/run/docker.sock:/var/run/docker.sock -v $HOME:/mnt/host murshies/devenv:latest
