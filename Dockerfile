# Example of running this in server mode:
# docker run -v /var/run/docker.sock:/var/run/docker.sock  -p $ssh_port:22 --name "$container_name" -d devenv:latest /usr/local/bin/server-mode.sh
FROM debian:12

ENV USERNAME=user
ENV USER=${USERNAME}

RUN apt-get update && apt-get install -y python-is-python3 python3-pip python3-apt python3-venv sudo debconf-utils locales

ENV TZ=Etc/UTC
RUN ln -sf /usr/share/zoneinfo/${TZ} /etc/localtime && echo ${TZ} > /etc/timezone

RUN echo 'en_US.UTF-8 UTF-8' >> /etc/locale.gen && locale-gen

RUN echo keyboard-configuration keyboard-configuration/layout select 'English (US)' | sudo debconf-set-selections && \
    echo keyboard-configuration keyboard-configuration/layoutcode select 'us' | sudo debconf-set-selections
RUN sudo DEBIAN_FRONTEND=noninteractive apt-get install -y keyboard-configuration

RUN useradd -m -s /bin/bash -u 5000 -G sudo ${USERNAME}
COPY . /setup
WORKDIR /setup
RUN ./user-sudo-all.sh ${USERNAME}

RUN chown -R ${USERNAME}:${USERNAME} /setup
USER ${USERNAME}
ENV CLEANUP_SETUP_VENV=t
RUN ./setup.sh -c cli,docker,emacs,gui
ENV CLEANUP_SETUP_VENV=
RUN /usr/local/bin/bootstrap-user.sh
RUN ./user-sudo.sh ${USERNAME}
WORKDIR /home/${USERNAME}
