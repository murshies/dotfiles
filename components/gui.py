"""Component for installing an openbox-based graphical environment."""
import logging
import os
import os.path

from lib.consts import FILES_DIR, SCRIPTS_DIR, SKEL_DIR
from lib.platform_filters import debian_or_ubuntu
from lib.resource import OS, resource, ResourceManager
from lib.util import apt_install, root_copy, run_cmd

logger = logging.getLogger(__name__)

GUI_PACKAGES = [
    'openssh-server',
    'x2goserver',
    'openbox',
    'tint2',
    'xfce4-terminal',
    'rofi',
    'pcmanfm',
    'gpicview',
]
BIN_SCRIPTS = [
    'dind.sh',
]


@resource(name='install-gui-packages', os=debian_or_ubuntu)
def install_gui_packages_ubuntu():
    apt_install(*GUI_PACKAGES)


def run() -> None:
    """Run the gui component installation."""
    logger.info('Install packages')
    ResourceManager.run('install-gui-packages')

    logger.info("Copying scripts to %s", SCRIPTS_DIR)
    run_cmd(['mkdir', '-p', SCRIPTS_DIR])
    for bin_script in BIN_SCRIPTS:
        root_copy(FILES_DIR, SCRIPTS_DIR, bin_script)

    logger.info('Add config files to skeleton directory')

    openbox_skel_dir = os.path.join(SKEL_DIR, '.config', 'openbox')
    run_cmd(['sudo', 'mkdir', '-p', openbox_skel_dir])
    for script in ('autostart.sh', 'rc.xml'):
        root_copy(FILES_DIR, openbox_skel_dir, script)

    xfce4_skel_dir = os.path.join(SKEL_DIR, '.config', 'xfce4', 'terminal')
    run_cmd(['sudo', 'mkdir', '-p', xfce4_skel_dir])
    root_copy(FILES_DIR, xfce4_skel_dir, 'terminalrc')

    tint2rc_skel_dir = os.path.join(SKEL_DIR, '.config', 'tint2')
    run_cmd(['sudo', 'mkdir', '-p', tint2rc_skel_dir])
    root_copy(FILES_DIR, tint2rc_skel_dir, 'tint2rc')

    logger.info('Set dpi settings')
    xresources_file_name = os.path.join(os.environ['HOME'], '.Xresources')
    with open(xresources_file_name, 'w') as f:
        f.write('Xft.dpi: 96')
