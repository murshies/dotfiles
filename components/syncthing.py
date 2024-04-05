"""Component for installing syncthing"""
import logging
import sh
import textwrap

from lib.platform_filters import debian_or_ubuntu
from lib.resource import resource, ResourceManager
from lib.util import apt_install, get_net_file, write_root_file

logger = logging.getLogger(__name__)

@resource(name='install-syncthing', os=debian_or_ubuntu)
def install_syncthing_debian():
    logger.info('Install packages for syncthing download')
    apt_install('apt-transport-https', 'ca-certificates', 'curl')

    logger.info('Ensure that /etc/apt/keyrings exists')
    sh.sudo.mkdir('-p', '/etc/apt/keyrings')
    sh.sudo.chmod('0755', '/etc/apt/keyrings')

    logger.info('Add apt key for syncthing')
    # Syncthing does not an ASCII armored GPG key, so simply download the file.
    get_net_file('https://syncthing.net/release-key.gpg',
                 '/etc/apt/keyrings/syncthing-archive-keyring.gpg',
                 '0644')
    write_root_file(
        'deb [signed-by=/etc/apt/keyrings/syncthing-archive-keyring.gpg] '
        'https://apt.syncthing.net/ syncthing stable',
        '/etc/apt/sources.list.d/syncthing.list',
        '0644')
    # Prefer syncthing from this repo instead of any system package.
    write_root_file(
        textwrap.dedent('''
        Package: *
        Pin: origin apt.syncthing.net
        Pin-Priority: 990
        ''').strip(),
        '/etc/apt/preferences.d/syncthing',
        '0644')
    sh.sudo('apt-get', 'update')

    logger.info('Install syncthing from apt repo')
    apt_install('syncthing')

def run() -> None:
    """Run the syncthing component installation."""
    ResourceManager.run('install-syncthing')
