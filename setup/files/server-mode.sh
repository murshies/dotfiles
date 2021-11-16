#!/bin/bash

export PATH="$HOME/bin:$PATH"

pull-dotfiles.sh
echo $(whoami):$(whoami) | sudo chpasswd
ssh-server.sh
dind.sh
install-emacs-packages.sh

sleep infinity
