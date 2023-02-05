"""Definitions for defining installable resources."""

from collections import namedtuple
from enum import Enum
import functools
import logging
import platform
import sh
import typing

logger = logging.getLogger(__name__)

Resource = namedtuple('Resource', ['name', 'os', 'os_version', 'arch'])
Platform = namedtuple('Platform', ['os', 'os_version', 'arch'])

class UnknownOSError(Exception):
    def __init__(self, os: str):
        super().__init__(f'Unknown OS "{os}"')

class OS(Enum):
    @staticmethod
    def from_str(os: str):
        if os == 'ubuntu':
            return OS.UBUNTU
        raise UnknownOSError(os)

    UBUNTU = 1

def get_current_platform() -> Platform:
    return Platform(
        os=OS.from_str(str(sh.lsb_release('-si').strip().lower())),
        os_version=str(sh.lsb_release('-sr').strip()),
        arch=platform.machine()
    )

def res_any(_: str) -> bool:
    return True

def compare_resource_field(resource_value: str, platform_value: str) -> bool:
    if callable(resource_value):
        return resource_value(platform_value)
    return resource_value == platform_value

class ResourceManager:
    resources = {}
    current_platform = get_current_platform()

    @classmethod
    def add_resource(cls, res: Resource, func: typing.Callable):
        cls.resources[res] = func

    @classmethod
    def match(cls, res_name: str, platform: Platform) -> typing.List:
        matching = []
        return matching

    @classmethod
    def run(cls, res_name: str):
        for res in cls.resources:
            if (res_name == res.name and
                compare_resource_field(res.os, cls.current_platform.os) and
                compare_resource_field(res.os_version, cls.current_platform.os_version) and
                compare_resource_field(res.arch, cls.current_platform.arch)):
                logger.info('Calling %s', res)
                cls.resources[res]()


def resource(name: str, os=res_any, os_version=res_any, arch=res_any):
    def wrapper(func):
        res = Resource(name, os, os_version, arch)
        ResourceManager.add_resource(res, func)
        return func
    return wrapper


def ubuntu_gte_20(plat_version) -> bool:
    try:
        major_version = int(plat_version.split('.')[0])
        return major_version >= 20
    except ValueError:
        return False
