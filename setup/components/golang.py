"""Component for installing golang."""
import logging
import os
import sh

from . import nix

logger = logging.getLogger(__name__)


def run() -> None:
    """Run the golang component installation."""
    logger.info('Install golang')
    nix.pkg_install('go')

    # logger.info('Install gopls for user')
    # sh.go.get('golang.org/x/tools/gopls@latest',
    #           _env={**os.environ, 'GO111MODULE': 'on'})
