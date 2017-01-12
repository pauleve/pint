
import os
import subprocess

from .cfg import *
from .model import *
from .ui import *
from .tools import *

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

