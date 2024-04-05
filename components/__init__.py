"""
Main module point for all installation components.

The main purpose of this is to collect all components into an ordered
dictionary, so that they can be iterated over.

"""
from collections import OrderedDict

from . import cli
from . import docker
from . import emacs
from . import gcloud
from . import golang
from . import gui
from . import kubectl
from . import syncthing

COMPONENTS = OrderedDict([
    ('cli', cli.run),
    ('docker', docker.run),
    ('emacs', emacs.run),
    ('gcloud', gcloud.run),
    ('golang', golang.run),
    ('gui', gui.run),
    ('kubectl', kubectl.run),
    ('syncthing', syncthing.run),
])

ESSENTIAL_COMPONENTS = OrderedDict([
    ('cli', cli.run),
    ('docker', docker.run),
    ('emacs', emacs.run),
])
