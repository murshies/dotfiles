"""Component for installing the google cloud sdk."""
import logging

from . import nix

logger = logging.getLogger(__name__)


def run() -> None:
    """Run the gcloud component installation."""
    logger.info('Install the google-cloud-sdk package')
    nix.pkg_install('google-cloud-sdk')
