#!/bin/bash

username="$1"
uid="$2"

if [ "$username" == "" ]; then
    echo "Username is required. Usage: $0 <username> [<uid>]"
    exit 1
fi

uid_str=""
if [ "$uid" != "" ]; then
    uid_str="-u ${uid}"
fi

sudo useradd -m -s /bin/bash ${uid_str} -G sudo ${username}
echo "${username} ALL=(ALL) NOPASSWD: ALL" | sudo tee -a /etc/sudoers
sudo su - ${username} -c 'pull-dotfiles.sh'
sudo su - ${username} -c 'bootstrap-user.sh'
sudo su - ${username} -c 'dind.sh'
