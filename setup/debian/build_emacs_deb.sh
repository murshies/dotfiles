#!/bin/sh

DPKG_ROOT=/opt/emacs-murshies

. /setup/venv-setup.sh
/setup/setup.py emacs

pip install j2cli

# Create the debian control file based off the template
export emacs_version=$(cat emacs_version.txt)
export architecture=$(dpkg-architecture -q DEB_BUILD_ARCH)

mkdir -p $DPKG_ROOT/opt/emacs-murshies
mkdir -p $DPKG_ROOT/DEBIAN
j2 /setup/debian/control.j2 -o $DPKG_ROOT/DEBIAN/control
cp /setup/debian/postinst $DPKG_ROOT/DEBIAN/postinst
mv /opt/$emacs_version /opt/emacs-murshies/opt/emacs-murshies
dpkg-deb --build $DPKG_ROOT
cp /opt/emacs-murshies.deb /tmp/out
