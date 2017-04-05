
import os
import tempfile

CFG = {
    "output_dir": "gen",
    "dbg": False,
}
"""
Python module configuation:

* `output_dir`: directory to use for saving intermediary files.
* `dbg`: enable debug output (see also :py:func:`.enable_dbg`,
  :py:func:`.disable_dbg`, :py:func:`.dbg`).
"""

try:
    get_ipython()
    IN_IPYTHON = True
except NameError:
    IN_IPYTHON = False

def output_dir():
    """
    Creates the output directory and returns its path
    """
    if not os.path.exists(CFG["output_dir"]):
        os.makedirs(CFG["output_dir"])
    return CFG["output_dir"]


def new_output_file(ext=None, **tempargs):
    """
    Creates a new file in :py:func:`output_dir` using `tempfile.mkstemp
    <https://docs.python.org/3/library/tempfile.html#tempfile.mkstemp>`_ and
    returns its path.
    The parameter `ext` specifies the extension of the file;
    `tempargs` are forwarded to ``tempfile.mkstemp`` with ``prefix=pint`` by
    default.
    """
    if "prefix" not in tempargs:
        tempargs["prefix"] = "pint"
    if ext is not None:
        tempargs["suffix"] = "%s.%s" % (tempargs.get("suffix", ""), ext)
    _, filename = tempfile.mkstemp(dir=output_dir(), **tempargs)
    return os.path.relpath(filename)

__all__ = [
    "CFG",
    "output_dir",
    "new_output_file",
    "IN_IPYTHON",
]

