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
from .consts import SKEL_DIR

COMPONENTS = OrderedDict([
    ('cli', cli.run),
    ('docker', docker.run),
    ('gcloud', gcloud.run),
    ('kubectl', kubectl.run),
    ('emacs', emacs.run),
    ('golang', golang.run),
    ('gui', gui.run),
])
