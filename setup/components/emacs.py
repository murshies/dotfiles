"""Component for installing emacs."""
from contextlib import contextmanager
import logging
import os
import os.path
import sh
from typing import List

from lib.consts import FILES_DIR, SCRIPTS_DIR
from lib.resource import OS, resource, ResourceManager
from lib.util import apt_install, root_copy, write_root_file

EMACS_VERSION = '28.2'
EMACS_TOOLKIT = 'athena'  # For gtk, use gkt2
EMACS_TOOLKIT_PACKAGE = 'libxaw7-dev'  # For gtk, use libgtk2.0-dev
EMACS_SOURCE_ROOT = os.path.join('/', 'src', f'emacs-{EMACS_VERSION}')
EMACS_INSTALL_ROOT = os.path.join('/', 'opt', f'emacs-{EMACS_VERSION}')
EMACS_PACKAGE = os.environ.get('EMACS_PACKAGE', 'source')

EMACS_DESKTOP_ENTRY = """
[Desktop Entry]
Name=Emacs
GenericName=Text Editor
Comment=Edit text
MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
Exec=emacsclient -a '' --eval "(load-project-management)" -c %F
Icon={emacs_package}
Type=Application
Terminal=false
Categories=Development;TextEditor;
StartupWMClass=Emacs
Keywords=Text;Editor;

"""

logger = logging.getLogger(__name__)


def gcc_major_version() -> int:
    """
    Determine the installed default gcc major version.

    :return: The GCC major version.
    :rtype: int
    """
    version_output = sh.gcc('--version')
    full_version = version_output.split('\n')[0].split(' ')[-1]
    major_version = full_version.split('.')[0]
    return int(major_version)


def emacs_build_packages() -> List[str]:
    """
    Generate the list of packages to install to build emacs.

    :return: The list of package names.
    :rtype: List[str]
    """
    return [
        EMACS_TOOLKIT_PACKAGE,
        'autoconf',
        'build-essential',
        'cmake',
        'git',
        'libfreetype6-dev',
        f'libgccjit-{gcc_major_version()}-dev',
        'libgif-dev',
        'libgnutls28-dev',
        'libjansson-dev',
        'libjpeg-dev',
        'libncurses5-dev',
        'libpng-dev',
        'libtiff5-dev',
        'libtool',
        'libtool-bin',
        'libxft-dev',
        'libxml2-dev',
        'libxpm-dev',
        'texinfo',
    ]


@contextmanager
def cwd(directory: str) -> None:
    """Context manager for temporarily changing the working directory."""
    currdir = os.getcwd()
    try:
        os.chdir(directory)
        yield
    finally:
        os.chdir(currdir)


@resource(name='install-emacs-deps', os=OS.UBUNTU)
def install_emacs_deps_ubuntu():
    build_deps = [
        EMACS_TOOLKIT_PACKAGE,
        'autoconf',
        'build-essential',
        'cmake',
        'git',
        'libfreetype6-dev',
        f'libgccjit-{gcc_major_version()}-dev',
        'libgif-dev',
        'libgnutls28-dev',
        'libjansson-dev',
        'libjpeg-dev',
        'libncurses5-dev',
        'libpng-dev',
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
        sh.sudo.rm('-rf', directory)
        sh.sudo.mkdir('-p', '-m', '0755', directory)

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
                '-sf',
                os.path.join(EMACS_INSTALL_ROOT, 'bin', exe),
                os.path.join('/usr/bin', exe))

    logger.info('Remove emacs source directory to conserve space')
    sh.sudo.rm('-rf', EMACS_SOURCE_ROOT)

    logger.info('Install emacs icons')
    build_icons_dir = os.path.join(EMACS_INSTALL_ROOT, 'share', 'icons', '.')
    sh.sudo.cp('-r', build_icons_dir, '/usr/share/icons')


def run() -> None:
    """Run the emacs component installation."""
    emacs_from_source()

    logger.info('Copy install-emacs-packages.sh to %s', SCRIPTS_DIR)
    sh.sudo.mkdir('-p', SCRIPTS_DIR)
    root_copy(FILES_DIR, SCRIPTS_DIR, 'install-emacs-packages.sh')

    logger.info('Create emacs.desktop file in applications directory')
    emacs_package = 'emacs' if EMACS_PACKAGE == 'source' else EMACS_PACKAGE
    emacs_desktop = EMACS_DESKTOP_ENTRY.format(
        emacs_package=emacs_package
    )
    write_root_file(
        emacs_desktop,
        '/usr/share/applications/emacs.desktop',
        '0644'
    )
