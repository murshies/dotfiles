"""Component for installing golang."""
import logging
import os
import sh

from .util import apt_install

logger = logging.getLogger(__name__)


def run() -> None:
    """Run the golang component installation."""
    logger.info('Install golang')
    apt_install('golang')

    logger.info('Install gopls for user')
    sh.go.get('golang.org/x/tools/gopls@latest',
              _env={**os.environ, 'GO111MODULE': 'on'})
