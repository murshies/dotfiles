#!/bin/bash

cp -r /etc/skel/. $HOME
echo $(whoami):$(whoami) | sudo chpasswd
install-emacs-packages.sh
