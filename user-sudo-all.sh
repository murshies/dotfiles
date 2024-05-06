#!/bin/bash

target_user="$1"
if [ -z "$target_user" ]; then
    target_user=$(whoami)
fi

cat << EOF | sudo tee /etc/sudoers.d/${target_user}
${target_user}	ALL=(ALL) NOPASSWD: ALL
EOF
