"""Component for installing docker."""
import logging
import os
import sh

from .util import apt_install

logger = logging.getLogger(__name__)


def run():
    """Run the docker component installation."""
    logger.info('Installing docker')
    apt_install('docker.io')

    logger.info('Add user to docker group')
    if os.environ['USER'] != 'root':
        sh.sudo.usermod('-a', '-G', 'docker', os.environ['USER'])
