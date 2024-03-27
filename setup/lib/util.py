"""Module for utility functions shared across components."""
import os
import sh
import typing


def apt_install(*packages: typing.List[str]) -> None:
    """
    Install a set of packages.

    :param packages: The list of packages.
    """
    sh.sudo('apt-get', 'install', '-y', *packages)


def write_root_file(contents: str, dst: str, mode: str=None) -> None:
    """
    Write a file as root.

    :param contents: The contents of the file.
    :param dst: The destination file name.
    :param mode: The mode of the dst file. This is optional.
    """
    sh.sudo.tee(sh.echo(contents), dst)
    if mode:
        sh.sudo.chmod(mode, dst)


def get_net_file(url: str, dst: str, mode: str=None) -> None:
    """
    Retrieve a file from the network and write it to disk.

    :param url: The URL of the file.
    :param dst: The destination file name.
    :param mode: The mode of the dst file. This is optional.
    """
    sh.sudo.tee(sh.curl('-L', '--output', '-', url), dst)
    if mode:
        sh.sudo.chmod(mode, dst)


def get_gpg_key(url: str, dst: str) -> None:
    """
    Download a GPG key from the network and dearmor it.

    :param url: The URL of the GPG key
    :param dst: The location on disk to put the GPG key
    """
    sh.sudo.gpg(sh.curl('-L', '--output', '-', url),
                '--batch', '--yes', '-o', dst, '--dearmor')
    sh.sudo.chmod('0644', dst)


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
    sh.sudo.cp(os.path.join(src, filename),
               os.path.join(dst, filename))
