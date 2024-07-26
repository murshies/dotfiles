#!/usr/bin/env python3
"""The main entrypoint for environment setup."""

import argparse
import logging
import os
import sys

from components import COMPONENTS, ESSENTIAL_COMPONENTS
from lib.consts import SKEL_DIR
from lib.util import run_cmd
from lib.platform_filters import debian_or_ubuntu
from lib.resource import resource, ResourceManager


def get_args() -> argparse.Namespace:
    """
    Parse the command line arguments.

    :return argparse.Namespace: The object containing the parsed parameters.
    """
    parser = argparse.ArgumentParser()
    parser.add_argument(
        '-c', '--components', required=False, default=None,
        help=('A common-separated list of components to run. If not '
              'specified, run only essential components.'))
    parser.add_argument(
        '-a', '--all-components', dest='all_components', required=False,
        action='store_true', default=False,
        help='When specified, run all components. This overrides -c.')
    parser.add_argument(
        '-l', '--list-components', dest='list_components', required=False,
        action='store_true', default=False,
        help='When specified, list all components to run and exit.')
    return parser.parse_args()


@resource(name='init-package-upgrade', os=debian_or_ubuntu)
def init_package_upgrade_debian():
    run_cmd(['sudo', 'apt-get', 'update'])
    run_cmd(['sudo', 'apt-get', 'upgrade', '-y'])


def main() -> int:
    """
    Run the main function for this script.

    :return int: The return code.
    """
    logging.basicConfig(
        level=logging.INFO,
        format=('%(asctime)s %(name)s.%(funcName)s:%(lineno)d '
                '::%(levelname)s: %(message)s'),
    )
    args = get_args()
    logger = logging.getLogger('setup')

    if args.list_components:
        logger.info('List of components:\n%s', '\n'.join(COMPONENTS.keys()))
        return 0

    logger.info('Doing initial package update and upgrade')
    ResourceManager.run('init-package-upgrade')

    logger.info("Adding $HOME/bin to $PATH if it hasn't been added already")
    profile_file_name = os.path.join(os.environ['HOME'], '.profile')
    with open(profile_file_name, 'r') as f:
        profile_lines = f.readlines()
    bin_path_line = 'export PATH="$HOME/bin:$PATH"'
    found_bin_path_line = any([
        bin_path_line in line
        for line in profile_lines
    ])
    if not found_bin_path_line:
        profile_lines.append(f'{bin_path_line}\n')
    with open(profile_file_name, 'w') as f:
        f.writelines(profile_lines)

    logger.info('Remove any existing skeleton directory')
    run_cmd(['sudo', 'rm', '-rf', SKEL_DIR])

    components_to_run = ESSENTIAL_COMPONENTS.keys()
    if args.all_components:
        components_to_run = COMPONENTS.keys()
    elif args.components:
        components_to_run = [
            component.strip() for component in args.components.split(',')
        ]

    logger.info('Running components:\n%s', '\n'.join(components_to_run))

    for component_name in components_to_run:
        if component_name not in COMPONENTS:
            continue
        component_exe = COMPONENTS[component_name]
        logger.info('Running setup for %s', component_name)
        component_exe()
    return 0


if __name__ == '__main__':
    sys.exit(main())
