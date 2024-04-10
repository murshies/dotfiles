"""Component for installing Mullvad."""
import logging
import sh
import textwrap

from lib.platform_filters import debian_or_ubuntu
from lib.resource import OS, resource, ResourceManager
from lib.util import apt_install, get_gpg_key, write_root_file

logger = logging.getLogger(__name__)

@resource(name='install-mullvad', os=debian_or_ubuntu)
def install_mullvad_debian():
    logger.info('Install packages for mullvad download')
    apt_install('apt-transport-https', 'ca-certificates', 'curl')

    logger.info('Ensure that /etc/apt/keyrings exists')
    sh.sudo.mkdir('-p', '/etc/apt/keyrings')
    sh.sudo.chmod('0755', '/etc/apt/keyrings')

    logger.info('Add apt key for mullvad')
    get_gpg_key('https://repository.mullvad.net/deb/mullvad-keyring.asc',
                '/etc/apt/keyrings/mullvad.gpg')
    dpkg_arch = sh.dpkg('--print-architecture').strip()
    dpkg_repo = sh.lsb_release('-cs').strip()
    write_root_file(
        f'deb [signed-by=/etc/apt/keyrings/mullvad.gpg arch={dpkg_arch}] '
        f'https://repository.mullvad.net/deb/stable {dpkg_repo} main',
        '/etc/apt/sources.list.d/mullvad.list',
        '0644')
    sh.sudo('apt-get', 'update')

    logger.info('Install mullvad from apt repo')
    apt_install('mullvad-vpn')

def run() -> None:
    """Run the mullvad component installation."""
    ResourceManager.run('install-mullvad')
