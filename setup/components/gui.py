"""Component for installing an openbox-based graphical environment."""
import logging
import os
import os.path
import sh

from lib.consts import FILES_DIR, SCRIPTS_DIR, SKEL_DIR
from lib.resource import OS, resource, ResourceManager
from lib.util import apt_install, root_copy

logger = logging.getLogger(__name__)

GUI_PACKAGES = [
    'openssh-server',
    'x2goserver',
    'openbox',
    'tint2',
    'xfce4-terminal',
    'rofi',
    'pcmanfm',
]
BIN_SCRIPTS = [
    'dind.sh',
]


@resource(name='install-gui-packages', os=OS.UBUNTU)
def install_gui_packages_ubuntu():
    apt_install(*GUI_PACKAGES)


def run() -> None:
    """Run the gui component installation."""
    logger.info('Install packages')
    ResourceManager.run('install-gui-packages')

    logger.info("Copying scripts to %s", SCRIPTS_DIR)
    sh.mkdir('-p', SCRIPTS_DIR)
    for bin_script in BIN_SCRIPTS:
        root_copy(FILES_DIR, SCRIPTS_DIR, bin_script)

    logger.info('Add config files to skeleton directory')

    openbox_skel_dir = os.path.join(SKEL_DIR, '.config', 'openbox')
    sh.sudo.mkdir('-p', openbox_skel_dir)
    for script in ('autostart.sh', 'rc.xml'):
        root_copy(FILES_DIR, openbox_skel_dir, script)

    xfce4_skel_dir = os.path.join(SKEL_DIR, '.config', 'xfce4', 'terminal')
    sh.sudo.mkdir('-p', xfce4_skel_dir)
    root_copy(FILES_DIR, xfce4_skel_dir, 'terminalrc')

    tint2rc_skel_dir = os.path.join(SKEL_DIR, '.config', 'tint2')
    sh.sudo.mkdir('-p', tint2rc_skel_dir)
    root_copy(FILES_DIR, tint2rc_skel_dir, 'tint2rc')
