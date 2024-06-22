"""Module for utility functions shared across components."""
# from http.client import HTTPResponse
import logging
import os
import subprocess
import typing
from urllib.request import urlopen

logger = logging.getLogger(__name__)


def run_cmd(cmd, *args, **kwargs) -> bytes:
    logger.info('Running command: %s', ' '.join(cmd))
    return subprocess.check_output(cmd, *args, **kwargs)


def download_url(url) -> bytes:
    with urlopen(url) as response:
        code = response.getcode()
        if code != 200:
            raise Exception(f'Download of {url} failed with code {code}')
        return response.read()


def apt_install(*packages: typing.List[str]) -> None:
    """
    Install a set of packages.

    :param packages: The list of packages.
    """
    run_cmd(['sudo', 'apt-get', 'install', '-y'] + list(packages))


def write_root_file(contents: str, dst: str, mode: str=None) -> None:
    """
    Write a file as root.

    :param contents: The contents of the file.
    :param dst: The destination file name.
    :param mode: The mode of the dst file. This is optional.
    """
    run_cmd(['sudo', 'tee', dst], input=contents.encode())
    if mode:
        run_cmd(['sudo', 'chmod', mode, dst])


def get_net_file(url: str, dst: str, mode: str=None) -> None:
    """
    Retrieve a file from the network and write it to disk.

    :param url: The URL of the file.
    :param dst: The destination file name.
    :param mode: The mode of the dst file. This is optional.
    """
    file_contents = download_url(url)
    run_cmd(['sudo', 'tee', dst], input=file_contents)
    if mode:
        run_cmd(['sudo', 'chmod', mode, dst])


def get_gpg_key(url: str, dst: str) -> None:
    """
    Download a GPG key from the network and dearmor it.

    :param url: The URL of the GPG key
    :param dst: The location on disk to put the GPG key
    """
    gpg_key = download_url(url)
    run_cmd(['sudo', 'gpg', '--batch', '--yes', '-o', dst, '--dearmor'], input=gpg_key)
    run_cmd(['sudo', 'chmod', '0644', dst])


def root_copy(src: str, dst: str, filename: str) -> None:
    """
    Copy a file from one directory to another as root.

    :param src: The source directory
    :type src: str
    :param dst: The destination directory
    :type dst: str
    :param filename: The name of the file to copy
    :type filename: str
    """
    run_cmd(['sudo', 'cp',
             os.path.join(src, filename),
             os.path.join(dst, filename)])
