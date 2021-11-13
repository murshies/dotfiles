
import re
import sh

def apt_install(*packages) -> None:
    sh.sudo('apt-get', 'install', '-y', *packages)

def write_root_file(contents: str, dst: str, mode: str = None) -> None:
    sh.sudo.tee(sh.echo(contents), dst)
    if mode:
        sh.sudo.chmod(mode, dst)

def get_net_file(url: str, dst: str, mode: str = None) -> None:
    sh.sudo.tee(sh.curl('--output', '-', url), dst)
    if mode:
        sh.sudo.chmod(mode, dst)
