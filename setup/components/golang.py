"""Component for installing golang."""
import logging
import os
import sh

from .consts import FILES_DIR, SCRIPTS_DIR
from .util import apt_install, root_copy

logger = logging.getLogger(__name__)


def run() -> None:
    """Run the golang component installation."""
    logger.info('Install golang')
    apt_install('golang')
