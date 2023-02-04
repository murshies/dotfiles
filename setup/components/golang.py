"""Component for installing golang."""
import logging
import os
import sh

from lib.consts import FILES_DIR, SCRIPTS_DIR
from lib.resource import OS, resource, ResourceManager
from lib.util import apt_install, root_copy

logger = logging.getLogger(__name__)

@resource(name='install-golang', os=OS.UBUNTU)
def install_golang_ubuntu():
    apt_install('golang')


def run() -> None:
    """Run the golang component installation."""
    logger.info('Install golang')
    ResourceManager.run('install-golang')
