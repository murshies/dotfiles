"""Component for installing the Kubernetes CLI interface."""
import logging

from . import nix

logger = logging.getLogger(__name__)


def run() -> None:
    """Run the kubectl component installation."""
    logger.info('Install the kubectl package')
    nix.pkg_install('kubectl')
