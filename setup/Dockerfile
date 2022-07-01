FROM ubuntu:20.04

ARG username=user
ARG setup_user=setup
ENV USER=${setup_user}

RUN apt-get update && apt-get install -y python-is-python3 python3-pip python3-apt python3-venv sudo debconf-utils

ENV TZ=Etc/UTC
RUN ln -sf /usr/share/zoneinfo/${TZ} /etc/localtime && echo ${TZ} > /etc/timezone

ENV DEBIAN_FRONTEND=noninteractive
RUN echo keyboard-configuration keyboard-configuration/layout select 'English (US)' | sudo debconf-set-selections && \
    echo keyboard-configuration keyboard-configuration/layoutcode select 'us' | sudo debconf-set-selections
RUN sudo apt-get install -y keyboard-configuration

RUN useradd -m -s /bin/bash -u 12345 -G sudo ${setup_user}
RUN echo "${setup_user} ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
# Copy requirements.txt separately first in order to cache the pip install
COPY . /setup
WORKDIR /setup

RUN chown -R ${setup_user}:${setup_user} /setup
USER ${setup_user}
ENV EMACS_PPA=true
ENV EMACS_PACKAGE=source
RUN source ./venv-setup.sh && ./setup.py
# WORKDIR /home/${setup_user}
ENV RUNTIME_USER=${username}
USER root
ENTRYPOINT ["/setup/entrypoint.sh"]
