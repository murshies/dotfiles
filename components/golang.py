"""Component for installing golang."""
import logging
import os

from lib.consts import FILES_DIR, SCRIPTS_DIR
from lib.platform_filters import debian_or_ubuntu
from lib.resource import OS, resource, ResourceManager
from lib.util import apt_install, download_url, root_copy, run_cmd

GO_VERSION = '1.23.4'

logger = logging.getLogger(__name__)

def install_golang(go_version: str, arch: str) -> None:
    """
    Install the target version of Golang on the system.

    :param go_version: The target semantic version
    :param arch: The system architecture. This should be the arch string as
        specified in the tar download URL.
    """
    run_cmd(['sudo', 'rm', '-rf', '/usr/local/go'])
    golang_tar = download_url(f'https://go.dev/dl/go{go_version}.linux-{arch}.tar.gz')
    run_cmd(['sudo', 'tar', '-C', '/usr/local', '-xzf', '-'],
            input=golang_tar)
    go_bins = [f.strip() for f in run_cmd(['ls', '/usr/local/go/bin']).decode('utf-8').split()]
    for go_bin in go_bins:
        run_cmd(['sudo', 'ln', '-sf', f'/usr/local/go/bin/{go_bin}', f'/usr/local/bin/{go_bin}'])


@resource(name='install-golang', arch='x86_64')
def install_golang_x86_64():
    install_golang(GO_VERSION, 'amd64')


@resource(name='install-golang', arch='aarch64')
def install_golang_arm64():
    install_golang(GO_VERSION, 'arm64')


def run() -> None:
    """Run the golang component installation."""
    logger.info('Install golang')
    ResourceManager.run('install-golang')
