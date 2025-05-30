FROM debian:12

ARG USERNAME=user
ENV USER=${USERNAME}

RUN apt-get update && apt-get install -y python-is-python3 sudo debconf-utils locales lsb-release

ENV TZ=Etc/UTC
RUN ln -sf /usr/share/zoneinfo/${TZ} /etc/localtime && echo ${TZ} > /etc/timezone

RUN echo 'en_US.UTF-8 UTF-8' >> /etc/locale.gen && locale-gen

RUN echo keyboard-configuration keyboard-configuration/layout select 'English (US)' | sudo debconf-set-selections && \
    echo keyboard-configuration keyboard-configuration/layoutcode select 'us' | sudo debconf-set-selections
RUN sudo DEBIAN_FRONTEND=noninteractive apt-get install -y keyboard-configuration

ARG UID=1000
ARG GID=1000
RUN groupadd -g ${GID} ${USERNAME} && \
    useradd -m -s /bin/bash -u ${UID} -g ${GID} -G sudo ${USERNAME}
COPY . /setup
WORKDIR /setup
RUN ./user-sudo-all.sh ${USERNAME}

RUN chown -R ${USERNAME}:${USERNAME} /setup
USER ${USERNAME}
ARG components=cli,docker,emacs,gui
RUN ./setup.sh -c ${components} && \
    bash -c 'source /setup/.bashrc && \
    bootstrap-user && \
    cd /setup && \
    link-dotfiles && \
    echo $(whoami):$(whoami) | sudo chpasswd' && \
    ./user-sudo.sh ${USERNAME}
WORKDIR /home/${USERNAME}
