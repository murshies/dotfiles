#!/bin/bash
# Script to build the latest emacs. Note that this is geared specifically
# towards building it on Ubuntu 18.04.

# so emacs server doesn't crash when the display closes. For gtk use gtk2. For
# athena use athena
TOOLKIT=athena
# For gtk use libgtk2.0-dev, for anthena use libxaw7-dev
TOOLKIT_PACKAGE=libxaw7-dev
VERSION=26.3
SRC_ROOT=/src
INSTALL_DEST=/opt/emacs-$VERSION

# Filesystem setup
mkdir $SRC_ROOT
chmod 777 $SRC_ROOT

# Get the latest emacs
wget -P /tmp https://ftp.gnu.org/gnu/emacs/emacs-$VERSION.tar.gz
tar -xvf /tmp/emacs-$VERSION.tar.gz -C $SRC_ROOT
rm /tmp/emacs-$VERSION.tar.gz

# Install dependencies
apt-get install -y $TOOLKIT_PACKAGE build-essential libgnutls28-dev libncurses5-dev libxpm-dev libjpeg-dev libtiff5-dev libpng-dev libgif-dev libxml2-dev libxft-dev libfreetype6-dev libjansson-dev

# Build and install emacs
cd $SRC_ROOT/emacs-$VERSION
./configure --prefix=$INSTALL_DEST --with-x-toolkit=$TOOLKIT
make -j $(nproc)
make install

for bin in emacs emacsclient; do
    ln -sf $INSTALL_DEST/bin/$bin /usr/bin/$bin
done
