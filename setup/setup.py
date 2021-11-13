#!/usr/bin/env python3

import argparse
import logging
import os
import sh
import sys

from components import COMPONENTS

def get_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()

    return parser.parse_args()

def main() -> int:
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s %(name)s.%(funcName)s:%(lineno)d ::%(levelname)s: %(message)s',
    )
    logger = logging.getLogger('setup')
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

    for component_name, component_exe in COMPONENTS.items():
        logger.info('Running setup for %s', component_name)
        component_exe()
    return 0

if __name__ == '__main__':
    sys.exit(main())
