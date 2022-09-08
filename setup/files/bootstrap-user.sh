#!/bin/bash

sudo cp -r /etc/skel/. $HOME
echo $(whoami):$(whoami) | sudo chpasswd
dind.sh
install-gopls.sh
install-emacs-packages.sh
