"""Component for installing the google cloud sdk."""
import logging
import sh

from lib.platform_filters import debian_or_ubuntu
from lib.resource import OS, resource, ResourceManager
from lib.util import apt_install, get_gpg_key, write_root_file

GCLOUD_DOWNLOAD_PACKAGES = [
    'apt-transport-https',
    'ca-certificates',
    'curl'
]

logger = logging.getLogger(__name__)

@resource(name='install-gcloud', os=debian_or_ubuntu)
def install_gcloud_ubuntu():
    logger.info('Install packages for gcloud download')
    apt_install(*GCLOUD_DOWNLOAD_PACKAGES)

    logger.info('Ensure that /etc/apt/keyrings exists')
    sh.sudo.mkdir('-p', '/etc/apt/keyrings')
    sh.sudo.chmod('0755', '/etc/apt/keyrings')

    logger.info('Add apt key for Google Cloud')
    get_gpg_key('https://packages.cloud.google.com/apt/doc/apt-key.gpg',
                 '/etc/apt/keyrings/cloud.google.gpg')

    logger.info('Add Cloud SDK package source')
    write_root_file('deb [signed-by=/etc/apt/keyrings/cloud.google.gpg] '
                    'https://packages.cloud.google.com/apt cloud-sdk main',
                    '/etc/apt/sources.list.d/google-cloud-sdk.list',
                    '0644')

    logger.info('Update apt cache and install google-cloud-sdk package')
    sh.sudo('apt-get', 'update')
    apt_install('google-cloud-sdk')


def run() -> None:
    """Run the gcloud component installation."""
    ResourceManager.run('install-gcloud')
