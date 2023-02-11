FROM debian:11

ENV USERNAME=user
ENV USER=${USERNAME}

RUN apt-get update && apt-get install -y python-is-python3 python3-pip python3-apt python3-venv sudo debconf-utils

ENV TZ=Etc/UTC
RUN ln -sf /usr/share/zoneinfo/${TZ} /etc/localtime && echo ${TZ} > /etc/timezone

RUN echo keyboard-configuration keyboard-configuration/layout select 'English (US)' | sudo debconf-set-selections && \
    echo keyboard-configuration keyboard-configuration/layoutcode select 'us' | sudo debconf-set-selections
RUN sudo DEBIAN_FRONTEND=noninteractive apt-get install -y keyboard-configuration

RUN useradd -m -s /bin/bash -u 5000 -G sudo ${USERNAME}
RUN echo "${USERNAME} ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
COPY . /setup
WORKDIR /setup

RUN chown -R ${USERNAME}:${USERNAME} /setup
USER ${USERNAME}
ENV CLEANUP_SETUP_VENV=t
RUN ./setup.sh
WORKDIR /home/${USERNAME}
