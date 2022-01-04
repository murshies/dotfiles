"""Functions for installing/using nix."""
from contextlib import contextmanager
import logging
import os
import requests
import sh

from .util import apt_install

NIX_INSTALL_SCRIPT_NAME = 'nix-install.sh'
logger = logging.getLogger(__name__)


@contextmanager
def daemon() -> None:
    """Context manager for running the nix daemon."""
    if os.environ.get('DOCKER', '').lower() == 'true':
        proc = sh.sudo(
            '/nix/var/nix/profiles/default/bin/nix-daemon', '--daemon',
            _bg=True)
        yield
        # The method proc.kill() does exist but it doesn't work in this case
        # since the process is run as root instead of the user.
        sh.sudo.kill(proc.pid)
    else:
        yield


def pkg_install(*packages, root: bool = False) -> None:
    """Install one or more nix packages."""
    package_str = ' '.join(packages)
    sudo = 'sudo ' if root else ''
    sh.bash(
        '-l', '-c',
        f'{sudo}/nix/var/nix/profiles/default/bin/nix-env -i {package_str}')


def install() -> None:
    """Run the nix component installation."""
    apt_install('curl')
    nix_script_response = requests.get('https://nixos.org/nix/install')
    nix_script_response.raise_for_status()
    with open(NIX_INSTALL_SCRIPT_NAME, 'w') as f:
        f.write(nix_script_response.text)
    sh.chmod('0755', NIX_INSTALL_SCRIPT_NAME)
    sh.sh(f'./{NIX_INSTALL_SCRIPT_NAME}', '--daemon')

    logger.info("Link the nix icons directory to the user's .icons directory")
    nix_icons_dir = os.path.join(
        os.environ['HOME'], '.nix-profile', 'share', 'icons')
    home_icons_dir = os.path.join(os.environ['HOME'], '.icons')
    sh.ln('-sf', nix_icons_dir, home_icons_dir)
