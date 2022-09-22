#!/usr/bin/env python3
"""The main entrypoint for environment setup."""

import argparse
import logging
import os
import sh
import sys

from components import COMPONENTS, SKEL_DIR


def get_args() -> argparse.Namespace:
    """
    Parse the command line arguments.

    :return argparse.Namespace: The object containing the parsed parameters.
    """
    parser = argparse.ArgumentParser()
    parser.add_argument(
        '-c', '--components', required=False, default=None,
        help=('A common-separated list of components to run. If not '
              'specified, run all components.'))
    parser.add_argument(
        '-l', '--list-components', dest='list_components', required=False,
        action='store_true', default=False,
        help='When specified, list all components to run and exit.')
    return parser.parse_args()


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
    sh.sudo('apt-get', 'update')
    sh.sudo('apt-get', 'upgrade', '-y')

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
    sh.sudo.rm('-rf', SKEL_DIR)

    components_to_run = COMPONENTS.keys()
    if args.components:
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
