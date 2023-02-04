"""Component for install cli packages."""
import logging
import os
import os.path
import sh

from lib.consts import FILES_DIR, SCRIPTS_DIR, SKEL_DIR
from lib.resource import OS, resource, ResourceManager, ubuntu_gte_20
from lib.util import apt_install, root_copy

logger = logging.getLogger(__name__)

SCRIPTS = [
    'aupdate',
    'bootstrap-user.sh',
    'install-gopls.sh',
    'new-user.sh',
    'pull-dotfiles.sh',
    'server-mode.sh',
]
SKEL_FILES = [
    '.bash_profile',
]
BASE_PACKAGES = [
    'apt-transport-https',
    'aspell',
    'ca-certificates',
    'curl',
    'git',
    'gnupg2',
    'htop',
    'ipcalc',
    'iputils-ping',
    'jq',
    'ncdu',
    'net-tools',
    'psmisc',
    'tmux',
    'traceroute',
    'unzip',
    'wget',
    'zip',
]

@resource(name='install-ripgrep', os=OS.UBUNTU, os_version=ubuntu_gte_20)
def install_ripgrep_ubuntu_gte_20():
    apt_install('ripgrep')


@resource(name='install-cli', os=OS.UBUNTU)
def install_cli_packages_ubuntu():
    apt_install(*BASE_PACKAGES)
    ResourceManager.run('install-ripgrep')


def run() -> None:
    """Run the cli component installation."""
    logger.info('Installing cli packages')
    ResourceManager.run('install-cli')

    logger.info('Copying scripts to %s', SCRIPTS_DIR)
    sh.sudo.mkdir('-p', SCRIPTS_DIR)
    for script in SCRIPTS:
        root_copy(FILES_DIR, SCRIPTS_DIR, script)

    logger.info('Copying files to %s', SKEL_DIR)
    sh.sudo.mkdir('-p', SKEL_DIR)
    for f in SKEL_FILES:
        root_copy(FILES_DIR, SKEL_DIR, f)
