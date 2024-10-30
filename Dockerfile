# Example of running this in server mode:
# docker run -v /var/run/docker.sock:/var/run/docker.sock  -p $ssh_port:22 --name "$container_name" -d $image_name sleep infinity
# Example of running this with X11 display sharing:
# docker run -v /var/run/docker.sock:/var/run/docker.sock -v /tmp/.X11-unix:/tmp/.X11-unix -e DISPLAY=$DISPLAY --name "$container_name" $image_name
FROM debian:12

ARG username=user
ENV USER=${username}

RUN apt-get update && apt-get install -y python-is-python3 sudo debconf-utils locales lsb-release

ENV TZ=Etc/UTC
RUN ln -sf /usr/share/zoneinfo/${TZ} /etc/localtime && echo ${TZ} > /etc/timezone

RUN echo 'en_US.UTF-8 UTF-8' >> /etc/locale.gen && locale-gen

RUN echo keyboard-configuration keyboard-configuration/layout select 'English (US)' | sudo debconf-set-selections && \
    echo keyboard-configuration keyboard-configuration/layoutcode select 'us' | sudo debconf-set-selections
RUN sudo DEBIAN_FRONTEND=noninteractive apt-get install -y keyboard-configuration

ARG user_id=5000
RUN useradd -m -s /bin/bash -u ${user_id} -G sudo ${username}
COPY . /setup
WORKDIR /setup
RUN ./user-sudo-all.sh ${username}

RUN chown -R ${username}:${username} /setup
USER ${username}
ARG components=cli,docker,emacs,gui
RUN ./setup.sh -c ${components} && \
    bash -c 'source /setup/.bashrc && \
    bootstrap-user && \
    cd /setup && \
    link-dotfiles && \
    echo $(whoami):$(whoami) | sudo chpasswd' && \
    ./user-sudo.sh ${username}
WORKDIR /home/${username}
