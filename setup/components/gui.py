
import logging
import os
import os.path
import sh

from .util import apt_install, write_root_file

logger = logging.getLogger(__name__)

BASE_PATH = os.path.join('.', 'files')
GUI_PACKAGES = [
    'openssh-server',
    'x2goserver',
    'openbox',
    'tint2',
    'xfce4-terminal',
]
KEYBOARD_LAYOUT = '''
# KEYBOARD CONFIGURATION FILE

# Consult the keyboard(5) manual page.

XKBMODEL="pc105"
XKBLAYOUT="us"
XKBVARIANT=""
XKBOPTIONS=""

BACKSPACE="guess"
'''

def run():
    logger.info('Install packages')
    apt_install(*GUI_PACKAGES)

    logger.info('Create openbox directories')
    openbox_dir = os.path.join(os.environ['HOME'], '.config', 'openbox')
    sh.mkdir('-p', openbox_dir)

    logger.info('Copy openbox scripts to openbox config directory')
    for script in ('autostart.sh', 'rc.xml'):
        sh.cp(os.path.join(BASE_PATH, script),
              os.path.join(openbox_dir, script))

    logger.info("Copy ssh-server.sh to user's home directory")
    user_bin = os.path.join(os.environ['HOME'], 'bin')
    sh.mkdir('-p', user_bin)
    sh.cp(os.path.join(BASE_PATH, 'ssh-server.sh'),
          os.path.join(user_bin, 'ssh-server.sh'))

    logger.info('Create xfce4-terminal config directory and copy config')
    xfce4terminal_dir = os.path.join(os.environ['HOME'], '.config', 'xfce4', 'terminal')
    sh.mkdir('-p', xfce4terminal_dir)
    sh.cp(os.path.join(BASE_PATH, 'terminalrc'),
          os.path.join(xfce4terminal_dir, 'terminalrc'))
