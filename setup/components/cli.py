"""Component for install cli packages."""
import logging
import os
import os.path
import sh

from .util import apt_install

logger = logging.getLogger(__name__)

DOTFILES_TO_ROOT = True
DOTFILES_REPO = 'https://github.com/murshies/dotfiles'
DOTFILES = [
    '.bashrc',
    '.emacs',
    '.inputrc',
    '.tmux.conf',
]
BASE_PATH = os.path.join('.', 'files')
SCRIPTS = [
    'aupdate',
    'pull-dotfiles.sh',
    'server-mode.sh',
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
    logger.info("Copying scripts to user's bin directory")
    user_bin = os.path.join(os.environ['HOME'], 'bin')
    sh.mkdir('-p', user_bin)
    for script in SCRIPTS:
        sh.cp(os.path.join(BASE_PATH, script),
              os.path.join(user_bin, script))
