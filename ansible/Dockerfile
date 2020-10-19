FROM ubuntu:20.04

RUN apt-get update && apt-get install -y python-is-python3 python3-pip python3-apt
# Copy requirements.txt separately first in order to cache the pip install
COPY requirements.txt /ansible/
WORKDIR /ansible
RUN pip3 install -r requirements.txt
COPY . /ansible
RUN ansible-playbook -i local site.yml
WORKDIR /root

ENTRYPOINT ["/bin/bash", "/ansible/entrypoint.sh"]
