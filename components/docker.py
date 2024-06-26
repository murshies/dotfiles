"""Component for installing docker."""
import logging
import os

from lib.platform_filters import debian_or_ubuntu
from lib.resource import OS, resource, ResourceManager
from lib.util import apt_install

logger = logging.getLogger(__name__)


@resource(name='install-docker', os=debian_or_ubuntu)
def install_docker_ubuntu():
    apt_install('docker.io')

def run():
    """Run the docker component installation."""
    logger.info('Installing docker')
    ResourceManager.run('install-docker')
