FROM ubuntu:20.04

ARG username=user
ENV USER=${username}
ENV DOCKER=true

RUN apt-get update && apt-get install -y python-is-python3 python3-pip python3-apt python3-venv sudo debconf-utils

ENV TZ=Etc/UTC
RUN ln -sf /usr/share/zoneinfo/${TZ} /etc/localtime && echo ${TZ} > /etc/timezone

ENV DEBIAN_FRONTEND=noninteractive
RUN echo keyboard-configuration keyboard-configuration/layout select 'English (US)' | sudo debconf-set-selections && \
    echo keyboard-configuration keyboard-configuration/layoutcode select 'us' | sudo debconf-set-selections
RUN sudo apt-get install -y keyboard-configuration

RUN useradd -m -s /bin/bash -G sudo ${username}
RUN echo "${username} ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
# Copy requirements.txt separately first in order to cache the pip install
COPY requirements.txt /setup/
WORKDIR /setup
# RUN pip3 install -r requirements.txt
COPY . /setup

RUN chown -R ${username}:${username} /setup
USER ${username}
RUN ./setup.sh
WORKDIR /home/${username}
