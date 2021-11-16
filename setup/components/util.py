"""Module for utility functions shared across components."""
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
    sh.sudo.tee(sh.curl('--output', '-', url), dst)
    if mode:
        sh.sudo.chmod(mode, dst)
