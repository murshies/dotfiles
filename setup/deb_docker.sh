#!/usr/bin/env bash

docker build -f Dockerfile_emacs_ubuntu_20.04 -t emacs-build .
docker run -v $PWD:/tmp/out emacs-build
