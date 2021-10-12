#!/bin/bash

cd $HOME
git clone https://github.com/murshies/dotfiles
cd $HOME/dotfiles
source .bashrc
link-dotfiles
