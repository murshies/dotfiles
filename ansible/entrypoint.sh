#!/bin/bash

if [ -z "$VNCPASSWD" ]; then
    echo 'Please specify the VNCPASSWD environment variable!'
    exit 1
fi

printf "$VNCPASSWD\n$VNCPASSWD\nn\n" | /usr/bin/vncpasswd
/usr/bin/tigervncserver -localhost no -xstartup /usr/bin/openbox-session
while true; do
    sleep 60
done
