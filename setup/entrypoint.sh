#!/bin/bash

export USER_UID=${LOCAL_UID:-5000}
export USER_GID=${LOCAL_GID:-$USER_UID}
groupadd -g $USER_GID $RUNTIME_USER
useradd -m -s /bin/bash -u $USER_UID -g $USER_GID -G sudo $RUNTIME_USER
echo "$RUNTIME_USER ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
for target in .config .icons bin go; do
    if [ -e /home/setup/$target ]; then
        cp -r /home/setup/$target /home/$RUNTIME_USER/$target
        chown -R $RUNTIME_USER:$RUNTIME_USER /home/$RUNTIME_USER/$target
    fi
done
gosu $RUNTIME_USER /setup/files/dind.sh
cd /home/$RUNTIME_USER
if [ "$#" -ne 0 ]; then
    exec gosu $RUNTIME_USER "$@"
else
    exec gosu $RUNTIME_USER /bin/bash -il
fi


