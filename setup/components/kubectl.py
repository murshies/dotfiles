"""Component for installing the Kubernetes CLI interface."""
import logging
import sh

from .util import apt_install, get_net_file, write_root_file

logger = logging.getLogger(__name__)


def run() -> None:
    """Run the kubectl component installation."""
    logger.info('Install packages for kubectl download')
    apt_install('apt-transport-https', 'ca-certificates', 'curl')

    logger.info('Add apt key for kubectl')
    get_net_file('https://packages.cloud.google.com/apt/doc/apt-key.gpg',
                 '/usr/share/keyrings/kubernetes-archive-keyring.gpg')
    write_root_file(
        'deb [signed-by=/usr/share/keyrings/kubernetes-archive-keyring.gpg] '
        'https://apt.kubernetes.io/ kubernetes-xenial main',
        '/etc/apt/sources.list.d/kubectl.list',
        '0644')
    sh.sudo('apt-get', 'update')

    logger.info('Install kubectl from apt repo')
    apt_install('kubectl')
