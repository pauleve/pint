"""
When loaded, the `pint` module will perform the follow tasks:

* add Pint third-party binary path (`bin` subdirectory of
  ``pint-config share-path``) to the ``PATH`` environment variable.
* if in IPython, displays the version of Pint binaries, and executes
  :py:func:`.ipython_install`.
"""
import os
import subprocess

from .cfg import *
from .types import *
from .model import *
from .ui import *
from .tools import *
from .utils import *

__SETUP_DONE = False

def setup_environ():
    global __SETUP_DONE
    share_path = subprocess.check_output(["pint-config", "share-path"]).decode()
    os.environ["PATH"] = "%s/bin:%s" % (share_path, os.environ["PATH"])
    __SETUP_DONE = True

def hello_ipython():
    version = subprocess.check_output(["pint-config", "version"]).decode()
    info("You are using Pint version %s" % version)


if not __SETUP_DONE:
    setup_environ()

if IN_IPYTHON:
    hello_ipython()
    from .ipython_helpers import ipython_install
    ipython_install()

