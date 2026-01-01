"""Component for installing emacs."""
from contextlib import contextmanager
import logging
import os
import os.path

from lib.platform_filters import debian_or_ubuntu
from lib.resource import resource, ResourceManager
from lib.util import apt_install, download_url, run_cmd

EMACS_VERSION = '30.2'
EMACS_TOOLKIT = 'athena'  # For gtk, use gkt2
EMACS_TOOLKIT_PACKAGE = 'libxaw7-dev'  # For gtk, use libgtk2.0-dev
EMACS_SOURCE_ROOT = os.path.join('/', 'src', f'emacs-{EMACS_VERSION}')
EMACS_INSTALL_ROOT = os.path.join('/', 'opt', f'emacs-{EMACS_VERSION}')
EMACS_KEEP_SOURCE = os.environ.get('EMACS_KEEP_SOURCE', 'true').lower() == 'true'

logger = logging.getLogger(__name__)


def gcc_major_version() -> int:
    """
    Determine the installed default gcc major version.

    :return: The GCC major version.
    :rtype: int
    """
    return int(run_cmd(['gcc', '-dumpversion']).decode('utf-8').strip())


@contextmanager
def cwd(directory: str) -> None:
    """Context manager for temporarily changing the working directory."""
    currdir = os.getcwd()
    try:
        os.chdir(directory)
        yield
    finally:
        os.chdir(currdir)


@resource(name='install-emacs-deps', os=debian_or_ubuntu)
def install_emacs_deps_debian():
    apt_install('build-essential')
    build_deps = [
        EMACS_TOOLKIT_PACKAGE,
        'autoconf',
        'cmake',
        'git',
        'libcairo2-dev',
        'libfreetype6-dev',
        f'libgccjit-{gcc_major_version()}-dev',
        'libgif-dev',
        'libgnutls28-dev',
        'libharfbuzz-dev',
        'libjansson-dev',
        'libjpeg-dev',
        'libncurses5-dev',
        'libpng-dev',
        'libsqlite3-dev',
        'libsystemd-dev',
        'libtiff5-dev',
        'libtool',
        'libtool-bin',
        'libxft-dev',
        'libxml2-dev',
        'libxpm-dev',
        'texinfo',
    ]
    apt_install(*build_deps)


def emacs_from_source() -> None:
    """Install emacs from source."""
    logger.info('Installing emacs from source')

    logger.info('Installing packages for building emacs')
    ResourceManager.run('install-emacs-deps')

    logger.info('Reset emacs source and install directories')
    for directory in (EMACS_SOURCE_ROOT, EMACS_INSTALL_ROOT):
        run_cmd(['sudo', 'rm', '-rf', directory])
        run_cmd(['sudo', 'mkdir', '-p', '-m', '0755', directory])

    logger.info('Download emacs source')

    emacs_tar = download_url(f'https://ftp.gnu.org/gnu/emacs/emacs-{EMACS_VERSION}.tar.gz')
    run_cmd(['sudo', 'tar', '-C', f'{EMACS_SOURCE_ROOT}/..', '-xzvf', '-'],
            input=emacs_tar)

    with cwd(EMACS_SOURCE_ROOT):
        logger.info('Run autogen.sh for emacs')
        run_cmd(['sudo', './autogen.sh'])

        logger.info('Configure emacs')
        run_cmd(['sudo', './configure',
                 '--with-native-compilation',
                 '--with-sqlite3',
                 '--with-libsystemd',
                 f'--prefix={EMACS_INSTALL_ROOT}',
                 f'--with-x-toolkit={EMACS_TOOLKIT}'])

        logger.info('Build emacs')
        nproc = run_cmd(['nproc']).decode('utf-8').strip()
        run_cmd(['sudo', 'make', '-j', nproc])

        logger.info('Install emacs to installation root')
        run_cmd(['sudo', 'make', 'install'])

        logger.info('Make emacs symlinks')
        for exe in ('emacs', 'emacsclient'):
            run_cmd(['sudo', 'ln',
                     '-sf',
                     os.path.join(EMACS_INSTALL_ROOT, 'bin', exe),
                     os.path.join('/usr/bin', exe)])

    if EMACS_KEEP_SOURCE:
        logger.info('Keeping emacs source directory')
    else:
        logger.info('Remove emacs source directory to conserve space')
        run_cmd(['sudo', 'rm', '-rf', EMACS_SOURCE_ROOT])

    logger.info('Install emacs icons')
    build_icons_dir = os.path.join(EMACS_INSTALL_ROOT, 'share', 'icons', '.')
    run_cmd(['sudo', 'cp', '-r', build_icons_dir, '/usr/share/icons'])
    emacsclient_desktop_file = os.path.join(
        EMACS_INSTALL_ROOT, 'share', 'applications', 'emacsclient.desktop')
    run_cmd(['sudo', 'cp', emacsclient_desktop_file, '/usr/share/applications/emacs.desktop'])


def run() -> None:
    """Run the emacs component installation."""
    emacs_from_source()
