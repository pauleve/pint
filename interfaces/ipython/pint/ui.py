
from .cfg import *

def enable_dbg():
    """
    Enable debug output (executed command lines, etc.)
    """
    CFG["dbg"] = True
def disable_dbg():
    """
    Disable debug output (executed command lines, etc.)
    """
    CFG["dbg"] = False

def dbg(msg):
    """
    If debug is enabled, print `msg` (prefixed with '#')
    """
    if CFG["dbg"]:
        print("# %s" % msg)

if IN_IPYTHON:
    from IPython.display import display, Markdown
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

