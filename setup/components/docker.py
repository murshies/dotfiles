"""Component for installing docker."""
import logging
import os
import sh

logger = logging.getLogger(__name__)


def run():
    """Run the docker component installation."""
    logger.info('Installing docker')
    sh.sudo('apt-get', 'install', '-y', 'docker.io')

    logger.info('Add user to docker group')
    if os.environ['USER'] != 'root':
        sh.sudo.usermod('-a', '-G', 'docker', os.environ['USER'])
