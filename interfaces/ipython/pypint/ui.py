
import os

from .cfg import *

if IN_IPYTHON:
    from colomoto_jupyter import disp_jupyter_js
    from IPython.display import display, Markdown, HTML

def enable_dbg():
    """
    Enable debug output (executed command lines, etc.)
    """
    CFG["dbg"] = True
    os.environ["OCAMLRUNPARAM"] = "b"
    if IN_IPYTHON:
        disp_jupyter_js("pypint_jsapi.debug_enabled(true);")

def disable_dbg():
    """
    Disable debug output (executed command lines, etc.)
    """
    CFG["dbg"] = False
    del os.environ["OCAMLRUNPARAM"]
    if IN_IPYTHON:
        disp_jupyter_js("pypint_jsapi.debug_enabled(false);")

def dbg(msg):
    """
    If debug is enabled, print `msg` (prefixed with '#')
    """
    if CFG["dbg"]:
        print("# %s" % msg)

if IN_IPYTHON:
    def info(msg):
        """
        Display `msg` (interpreted in Markdown format)
        """
        display(Markdown(msg))
else:
    def info(msg):
        """
        Print `msg` (note: in IPython, `msg` is interpreted in Markdown
        format)
        """
        print(msg)

__all__ = [
    "dbg",
    "enable_dbg",
    "disable_dbg",
    "info"
]

