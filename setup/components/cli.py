"""Component for install cli packages."""
import logging
import os
import os.path
import sh

from .consts import FILES_DIR, SCRIPTS_DIR, SKEL_DIR
from .util import apt_install, root_copy

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
PACKAGES = [
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
    'ripgrep',
    'tmux',
    'traceroute',
    'unzip',
    'wget',
    'zip',
]


def run() -> None:
    """Run the cli component installation."""
    logger.info('Installing cli packages')
    apt_install(*PACKAGES)

    logger.info('Copying scripts to %s', SCRIPTS_DIR)
    sh.sudo.mkdir('-p', SCRIPTS_DIR)
    for script in SCRIPTS:
        root_copy(FILES_DIR, SCRIPTS_DIR, script)

    logger.info('Copying files to %s', SKEL_DIR)
    sh.sudo.mkdir('-p', SKEL_DIR)
    for f in SKEL_FILES:
        root_copy(FILES_DIR, SKEL_DIR, f)
