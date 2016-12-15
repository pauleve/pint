
import subprocess

from .cfg import *
from .model import *
from .ui import *
from .tools import *

def hello_ipython():
    version = subprocess.check_output(["pint-config", "version"]).decode()
    info("You are using Pint version %s" % version)


if IN_IPYTHON:
    hello_ipython()

