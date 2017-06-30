"""
When loaded, the `pypint` module will perform the follow tasks:

* add Pint third-party binary path (`bin` subdirectory of
  ``pint-config share-path``) to the ``PATH`` environment variable.
* if in IPython, displays the version of Pint binaries, and executes
  :py:func:`.ipython_install`.
"""
__version__ = "1.2.4"
__pint_required__ = "2017-06-30"

import os
import shutil
import subprocess
import sys

if sys.version_info[0] < 3:
    raise Exception("python >= 3 is required")

from .cfg import *
from .types import *
from .model import *
from .ui import *
from .tools import *
from .utils import *

__SETUP_DONE = False

__PINT_VERSION__ = None

def check_pint():
    if shutil.which("pint-config") is None:
        raise Exception("Pint binaries should be installed separately. " + \
            "See https://loicpauleve.name/pint/doc/#Binaries")

    global __PINT_VERSION__
    version = subprocess.check_output(["pint-config", "version"]).decode()
    __PINT_VERSION__ = version.strip()

    if __pint_required__ > __PINT_VERSION__:
        raise Exception("Pint >= {0} is required. Please upgrade Pint binaries. " \
                .format(__pint_required__) +  \
            "See https://loicpauleve.name/pint/doc/#Binaries" )


def setup_environ():
    check_pint()
    global __SETUP_DONE
    share_path = subprocess.check_output(["pint-config", "share-path"]).decode()
    bin_path = os.path.join(share_path.strip(), "bin")
    os.environ["PATH"] = "%s:%s" % (bin_path, os.environ["PATH"])
    __SETUP_DONE = True

def hello_ipython():
    info("You are using Pint version %s and pypint %s" % (__PINT_VERSION__,__version__))


if not __SETUP_DONE:
    setup_environ()

if IN_IPYTHON:
    hello_ipython()
    from .ipython_helpers import ipython_install
    ipython_install()

