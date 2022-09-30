#!/bin/bash

cp -r /etc/skel/. $HOME
echo $(whoami):$(whoami) | sudo chpasswd
install-gopls.sh
install-emacs-packages.sh
