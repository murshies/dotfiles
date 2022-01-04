"""Component for installing an openbox-based graphical environment."""
import logging
import os
import os.path
import sh

from .util import apt_install

logger = logging.getLogger(__name__)

BASE_PATH = os.path.join('.', 'files')
GUI_PACKAGES = [
    'openssh-server',
    'x2goserver',
    'openbox',
    'tint2',
    'xfce4-terminal',
    'rofi',
]
BIN_SCRIPTS = [
    'ssh-server.sh',
    'dind.sh',
]


def run() -> None:
    """Run the gui component installation."""
    logger.info('Install packages')
    apt_install(*GUI_PACKAGES)

    logger.info('Create openbox directories')
    openbox_dir = os.path.join(os.environ['HOME'], '.config', 'openbox')
    sh.mkdir('-p', openbox_dir)

    logger.info('Copy openbox scripts to openbox config directory')
    for script in ('autostart.sh', 'rc.xml'):
        sh.cp(os.path.join(BASE_PATH, script),
              os.path.join(openbox_dir, script))

    logger.info("Copy scripts to user's bin directory")
    user_bin = os.path.join(os.environ['HOME'], 'bin')
    sh.mkdir('-p', user_bin)
    for bin_script in BIN_SCRIPTS:
        sh.cp(os.path.join(BASE_PATH, bin_script),
              os.path.join(user_bin, bin_script))

    logger.info('Create xfce4-terminal config directory and copy config')
    xfce4terminal_dir = os.path.join(
        os.environ['HOME'], '.config', 'xfce4', 'terminal')
    sh.mkdir('-p', xfce4terminal_dir)
    sh.cp(os.path.join(BASE_PATH, 'terminalrc'),
          os.path.join(xfce4terminal_dir, 'terminalrc'))

    logger.info("Copy tint2rc to user's config directory")
    tint2rc_dir = os.path.join(os.environ['HOME'], '.config', 'tint2')
    sh.mkdir('-p', tint2rc_dir)
    sh.cp(os.path.join(BASE_PATH, 'tint2rc'),
          os.path.join(tint2rc_dir, 'tint2rc'))
