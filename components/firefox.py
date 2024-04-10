"""Component for installing Firefox."""
import logging
import sh
import textwrap

from lib.platform_filters import debian_or_ubuntu
from lib.resource import OS, resource, ResourceManager
from lib.util import apt_install, get_gpg_key, write_root_file

logger = logging.getLogger(__name__)

@resource(name='install-firefox', os=debian_or_ubuntu)
def install_firefox_debian():
    logger.info('Install packages for firefox download')
    apt_install('apt-transport-https', 'ca-certificates', 'curl')

    logger.info('Ensure that /etc/apt/keyrings exists')
    sh.sudo.mkdir('-p', '/etc/apt/keyrings')
    sh.sudo.chmod('0755', '/etc/apt/keyrings')

    logger.info('Add apt key for firefox')
    get_gpg_key('https:/packages.mozilla.org/apt/repo-signing-key.gpg',
                '/etc/apt/keyrings/mozilla.gpg')
    write_root_file(
        'deb [signed-by=/etc/apt/keyrings/mozilla.gpg] '
        'https://packages.mozilla.org/apt mozilla main',
        '/etc/apt/sources.list.d/mozilla.list',
        '0644')
    # Prefer firefox from this repo instead of any system package.
    write_root_file(
        textwrap.dedent('''
        Package: *
        Pin: origin packages.mozilla.org
        Pin-Priority: 990''').strip(),
        '/etc/apt/preferences.d/mozilla',
        '0644')
    sh.sudo('apt-get', 'update')

    logger.info('Install firefox from apt repo')
    apt_install('firefox')

def run() -> None:
    """Run the firefox component installation."""
    ResourceManager.run('install-firefox')
