from collections import OrderedDict

from . import cli
from . import docker
from . import emacs
from . import gcloud
from . import golang
from . import gui
from . import kubectl

COMPONENTS = OrderedDict([
    ('cli', cli.run),
    ('docker', docker.run),
    ('gcloud', gcloud.run),
    ('kubectl', kubectl.run),
    ('emacs', emacs.run),
    ('golang', golang.run),
    ('gui', gui.run),
])
