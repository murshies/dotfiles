"""Component for installing emacs."""
from contextlib import contextmanager
import logging
import os
import os.path
import sh

from .util import apt_install

BASE_PATH = os.path.join('.', 'files')
EMACS_VERSION = '27.2'
EMACS_TOOLKIT = 'athena'  # For gtk, use gkt2
EMACS_TOOLKIT_PACKAGE = 'libxaw7-dev'  # For gtk, use libgtk2.0-dev
EMACS_SOURCE_ROOT = os.path.join('/', 'src', f'emacs-{EMACS_VERSION}')
EMACS_INSTALL_ROOT = os.path.join('/', 'opt', f'emacs-{EMACS_VERSION}')
EMACS_PACKAGE = 'source'
EMACS_BUILD_PACKAGES = [
    EMACS_TOOLKIT_PACKAGE,
    'autoconf',
    'build-essential',
    'libgnutls28-dev',
    'libncurses5-dev',
    'libxpm-dev',
    'libjpeg-dev',
    'libtiff5-dev',
    'libpng-dev',
    'libgif-dev',
    'libxml2-dev',
    'libxft-dev',
    'libfreetype6-dev',
    'libjansson-dev',
    'cmake',
    'libtool',
    'libtool-bin',
    'texinfo',
    'git',
]

logger = logging.getLogger(__name__)


@contextmanager
def cwd(directory: str) -> None:
    """Context manager for temporarily changing the working directory."""
    currdir = os.getcwd()
    try:
        os.chdir(directory)
        yield
    finally:
        os.chdir(currdir)


def emacs_from_source() -> None:
    """Install emacs from source."""
    logger.info('Installing emacs from source')

    logger.info('Installing packages for building emacs')
    apt_install(*EMACS_BUILD_PACKAGES)

    logger.info('Reset emacs source and install directories')
    for directory in (EMACS_SOURCE_ROOT, EMACS_INSTALL_ROOT):
        sh.sudo.rm('-rf', directory)
        sh.sudo.mkdir('-p', directory)

    logger.info('Download emacs source')
    sh.sudo.git.clone(
        'https://git.savannah.gnu.org/git/emacs.git',
        EMACS_SOURCE_ROOT)

    with cwd(EMACS_SOURCE_ROOT):
        sh.sudo.git.checkout(f'emacs-{EMACS_VERSION}')
        logger.info('Run autogen.sh for emacs')
        sh.sudo('./autogen.sh')

        logger.info('Configure emacs')
        sh.sudo('./configure',
                '--with-native-compilation',
                f'--prefix={EMACS_INSTALL_ROOT}',
                f'--with-x-toolkit={EMACS_TOOLKIT}')

        logger.info('Build emacs')
        nproc = sh.nproc().strip()
        sh.sudo.make('-j', nproc)

        logger.info('Install emacs to installation root')
        sh.sudo.make.install()

        logger.info('Make emacs symlinks')
        for exe in ('emacs', 'emacsclient'):
            sh.sudo.ln(
                '-s',
                os.path.join(EMACS_INSTALL_ROOT, 'bin', exe),
                os.path.join('/', 'usr', 'bin', exe))

        logger.info('Remove emacs source directory to conserve space')

    sh.sudo.rm('-rf', EMACS_SOURCE_ROOT)

    logger.info("Copy the emacs icons to the user's .icons directory")
    build_icons_dir = os.path.join(EMACS_INSTALL_ROOT, 'share', 'icons')
    home_icons_dir = os.path.join(os.environ['HOME'], '.icons')
    sh.cp('-r', build_icons_dir, home_icons_dir)


def emacs_from_package() -> None:
    """Install emacs from the package manager."""
    logger.info('Installing emacs from package')
    apt_install(EMACS_PACKAGE)


def run() -> None:
    """Run the emacs component installation."""
    if EMACS_PACKAGE == 'source':
        emacs_from_source()
    else:
        emacs_from_package()

    logger.info("Copy install-emacs-packages.sh to user's bin directory")
    user_bin = os.path.join(os.environ['HOME'], 'bin')
    sh.mkdir('-p', user_bin)
    sh.cp(os.path.join(BASE_PATH, 'install-emacs-packages.sh'),
          os.path.join(user_bin, 'install-emacs-packages.sh'))

    logger.info('Copy modified emacs.desktop to applications directory')
    sh.sudo.cp(os.path.join(BASE_PATH, 'emacs.desktop'),
               '/usr/share/applications/emacs.desktop')
