#!/bin/bash

cd $HOME
rm -rf $HOME/dotfiles
git clone https://github.com/murshies/dotfiles
cd $HOME/dotfiles
source .bashrc
link-dotfiles
