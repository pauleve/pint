
from .cfg import *

def dbg(msg):
    print("# %s" % msg)

if IN_IPYTHON:
    from IPython.display import display, Markdown
    def info(msg):
        display(Markdown(msg))
else:
    def info(msg):
        print(msg)

__all__ = [
    "dbg",
    "info"
]

