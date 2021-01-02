FROM ubuntu:20.04

ARG username=user

RUN apt-get update && apt-get install -y python-is-python3 python3-pip python3-apt sudo
RUN useradd -m -s /bin/bash -G sudo ${username}
RUN echo "${username} ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
# Copy requirements.txt separately first in order to cache the pip install
COPY requirements.txt /ansible/
WORKDIR /ansible
RUN pip3 install -r requirements.txt
COPY . /ansible

RUN chown -R ${username}:${username} /ansible
USER ${username}
RUN ansible-playbook -i local site.yml
WORKDIR /home/${username}
ENTRYPOINT ["/bin/bash", "/ansible/entrypoint.sh"]
