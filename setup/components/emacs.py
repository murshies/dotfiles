"""Component for installing emacs."""
from contextlib import contextmanager
import logging
import os
import os.path
import sh
from typing import List

from .consts import FILES_DIR, SKEL_DIR
from .util import apt_install, root_copy, write_root_file

EMACS_VERSION = '28.1'
EMACS_TOOLKIT = 'athena'  # For gtk, use gkt2
EMACS_TOOLKIT_PACKAGE = 'libxaw7-dev'  # For gtk, use libgtk2.0-dev
EMACS_SOURCE_ROOT = os.path.join('/', 'src', f'emacs-{EMACS_VERSION}')
EMACS_INSTALL_ROOT = os.path.join('/', 'opt', f'emacs-{EMACS_VERSION}')
EMACS_PACKAGE = os.environ.get('EMACS_PACKAGE', 'source')
EMACS_PPA = os.environ.get('EMACS_PPA', '').lower() == 'true'

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


def emacs_from_source() -> None:
    """Install emacs from source."""
    logger.info('Installing emacs from source')

    logger.info('Installing packages for building emacs')
    apt_install(*emacs_build_packages())

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


def emacs_from_package() -> None:
    """Install emacs from the package manager."""
    logger.info('Installing emacs from package')
    if EMACS_PPA:
        add_emacs_ppa()
    apt_install(EMACS_PACKAGE)


def add_emacs_ppa() -> None:
    """Add extra PPA for emacs packages."""
    ppa_name = 'ppa:kelleyk/emacs'
    logger.info('Adding PPA %s for installing emacs', ppa_name)
    apt_install('software-properties-common')
    sh.sudo('add-apt-repository', ppa_name)
    sh.sudo.apt.update()


def run() -> None:
    """Run the emacs component installation."""
    if EMACS_PACKAGE == 'source':
        emacs_from_source()
    else:
        emacs_from_package()

    logger.info('Copy install-emacs-packages.sh to the skeleton directory')
    skel_bin_dir = os.path.join(SKEL_DIR, 'bin')
    sh.sudo.mkdir('-p', skel_bin_dir)
    root_copy(FILES_DIR, skel_bin_dir, 'install-emacs-packages.sh')

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
