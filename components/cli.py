"""Component for install cli packages."""
import logging
import os
import os.path
import shutil
import tempfile
from urllib.request import urlopen

from lib.consts import FILES_DIR, SCRIPTS_DIR, SKEL_DIR
import lib.platform_filters as pf
from lib.resource import OS, resource, ResourceManager
from lib.util import apt_install, root_copy, run_cmd

logger = logging.getLogger(__name__)

SKEL_FILES = [
    '.bash_profile',
]
BASE_PACKAGES = [
    'apt-transport-https',
    'aspell',
    'ca-certificates',
    'curl',
    'dnsutils',
    'git',
    'gnupg2',
    'htop',
    'ipcalc',
    'iputils-ping',
    'jq',
    'ncdu',
    'net-tools',
    'psmisc',
    'tmux',
    'traceroute',
    'unzip',
    'wget',
    'x2goclient',
    'zip',
]

@resource(name='install-ripgrep', os=OS.DEBIAN)
@resource(name='install-ripgrep', os=OS.UBUNTU, os_version=pf.ubuntu_gte_18_10)
def install_ripgrep_ubuntu_gte_18_10():
    apt_install('ripgrep')


@resource(name='install-ripgrep', os=OS.UBUNTU, os_version=pf.ubuntu_lt_18_10, arch='x86_64')
def install_ripgrep_ubuntu_lt_18_10_x86_64():
    download_url = 'https://github.com/BurntSushi/ripgrep/releases/download/13.0.0/ripgrep_13.0.0_amd64.deb'
    deb_file_name = 'ripgrep.deb'
    with urlopen(download_url) as response:
        code = response.getcode()
        if code != 200:
            raise Exception(f'Download of {download_url} failed with code {code}')
        try:
            with open(deb_file_name, 'wb') as f:
                shutil.copyfileobj(response, f)
            run_cmd(['sudo', 'dpkg', '-i', deb_file_name])
        finally:
            if os.path.exists(deb_file_name):
                os.remove(deb_file_name)


@resource(name='install-cli', os=pf.debian_or_ubuntu)
def install_cli_packages_ubuntu():
    apt_install(*BASE_PACKAGES)
    ResourceManager.run('install-ripgrep')


def run() -> None:
    """Run the cli component installation."""
    logger.info('Installing cli packages')
    ResourceManager.run('install-cli')

    logger.info('Copying files to %s', SKEL_DIR)
    run_cmd(['sudo', 'mkdir', '-p', SKEL_DIR])
    for f in SKEL_FILES:
        root_copy(FILES_DIR, SKEL_DIR, f)
