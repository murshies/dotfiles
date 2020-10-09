#!/bin/bash

sudo apt-get update && sudo apt-get install -y python-is-python3 python3-pip python3-apt
sudo pip3 install -r requirements.txt
ansible-playbook -i local site.yml
