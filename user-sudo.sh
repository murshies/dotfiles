#!/bin/bash

target_user="$1"
if [ -z "$target_user" ]; then
    target_user=$(whoami)
fi

cat << EOF | sudo tee /etc/sudoers.d/${target_user}
${target_user}	ALL=(ALL) ALL
${target_user}	ALL=(ALL) NOPASSWD:SETENV: /usr/bin/apt-get update, /usr/bin/apt-get upgrade*
EOF

