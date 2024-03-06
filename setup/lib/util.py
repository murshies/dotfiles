"""Module for utility functions shared across components."""
import os
import sh


def apt_install(*packages) -> None:
    """
    Install a set of packages.

    :param packages list[str]: The listo of packages.
    """
    sh.sudo('apt-get', 'install', '-y', *packages)


def write_root_file(contents: str, dst: str, mode: str = None) -> None:
    """
    Write a file as root.

    :param contents str: The contents of the file.
    :param dst str: The destination file name.
    :param mode str: The mode of the dst file. This is optional.
    """
    sh.sudo.tee(sh.echo(contents), dst)
    if mode:
        sh.sudo.chmod(mode, dst)


def get_net_file(url: str, dst: str, mode: str = None) -> None:
    """
    Retrieve a file from the network and write it to disk.

    :param url str: The URL of the file.
    :param dst str: The destination file name.
    :param mode str: The mode of the dst file. This is optional.
    """
    sh.sudo.tee(sh.curl('-L', '--output', '-', url), dst)
    if mode:
        sh.sudo.chmod(mode, dst)


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
