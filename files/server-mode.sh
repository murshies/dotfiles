#!/bin/bash

dind.sh
sudo service ssh start

if [ "$$" -eq 1 ]; then
    sleep infinity
fi
