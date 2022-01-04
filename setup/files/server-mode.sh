#!/bin/bash

export PATH="$HOME/bin:$PATH"

sudo /nix/var/nix/profiles/default/bin/nix-daemon --daemon &
pull-dotfiles.sh
echo $(whoami):$(whoami) | sudo chpasswd
ssh-server.sh
dind.sh
install-emacs-packages.sh

sleep infinity
