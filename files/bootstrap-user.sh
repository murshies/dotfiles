#!/bin/bash

cp -r /etc/skel/. $HOME
echo $(whoami):$(whoami) | sudo chpasswd
if [ -d /setup ]; then
    cd /setup
    source .bashrc
    link-dotfiles
fi
