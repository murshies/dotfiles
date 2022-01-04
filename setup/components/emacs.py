"""Component for installing emacs."""
import logging
import os
import os.path
import sh

from . import nix

BASE_PATH = os.path.join('.', 'files')
DESKTOP_FILE_NAME = 'emacs.desktop'
logger = logging.getLogger(__name__)


def run() -> None:
    """Run the emacs component installation."""
    nix.pkg_install('emacs')

    logger.info("Copy install-emacs-packages.sh to user's bin directory")
    user_bin = os.path.join(os.environ['HOME'], 'bin')
    sh.mkdir('-p', user_bin)
    sh.cp(os.path.join(BASE_PATH, 'install-emacs-packages.sh'),
          os.path.join(user_bin, 'install-emacs-packages.sh'))

    logger.info("Copy emacs.desktop to the user's applications directory")
    home_apps_dir = os.path.join(
        os.environ['HOME'], '.local', 'share', 'applications')
    sh.mkdir('-p', home_apps_dir)
    sh.cp(os.path.join(BASE_PATH, DESKTOP_FILE_NAME),
          os.path.join(home_apps_dir, DESKTOP_FILE_NAME))
