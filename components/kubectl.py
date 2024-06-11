"""Component for installing the Kubernetes CLI interface."""
import logging
import sh
import textwrap

from lib.platform_filters import debian_or_ubuntu
from lib.resource import OS, resource, ResourceManager
from lib.util import apt_install, get_gpg_key, write_root_file

logger = logging.getLogger(__name__)

KUBECTL_VERSION = '1.30'

@resource(name='install-kubectl', os=debian_or_ubuntu)
def install_kubectl_ubuntu():
    logger.info('Install packages for kubectl download')
    apt_install('apt-transport-https', 'ca-certificates', 'curl')

    logger.info('Ensure that /etc/apt/keyrings exists')
    sh.sudo.mkdir('-p', '/etc/apt/keyrings')
    sh.sudo.chmod('0755', '/etc/apt/keyrings')

    logger.info('Add apt key for kubectl')
    get_gpg_key(f'https://pkgs.k8s.io/core:/stable:/v{KUBECTL_VERSION}/deb/Release.key',
                '/etc/apt/keyrings/kubernetes-apt-keyring.gpg')
    write_root_file(
        'deb [signed-by=/etc/apt/keyrings/kubernetes-apt-keyring.gpg] '
        f'https://pkgs.k8s.io/core:/stable:/v{KUBECTL_VERSION}/deb/ /',
        '/etc/apt/sources.list.d/kubernetes.list',
        '0644')
    # Prefer kubectl from this repo instead of the Google Cloud repo.
    write_root_file(
        textwrap.dedent('''
        Package: kubectl
        Pin: origin pkgs.k8s.io
        Pin-Priority: 900
        ''').strip(),
        '/etc/apt/preferences.d/kubectl',
        '0644')
    sh.sudo('apt-get', 'update')

    logger.info('Install kubectl from apt repo')
    apt_install('kubectl')

def run() -> None:
    """Run the kubectl component installation."""
    ResourceManager.run('install-kubectl')
