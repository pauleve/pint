
import os
import tempfile

from colomoto_jupyter import IN_IPYTHON
from colomoto_jupyter.sessionfiles import \
    output_dir, \
    new_output_file, \
    remove_output_files

CFG = {
    "dbg": False,
    "console": False,
}
"""
Python module configuation:

* `dbg`: enable debug output (see also :py:func:`.enable_dbg`,
  :py:func:`.disable_dbg`, :py:func:`.dbg`).
* `console`: assume console output
"""

__DATAFILES_DIR__ = os.path.join(os.path.dirname(__file__), "files")

def data_file(name):
    """
    Returns path to file `name` distributed with ``pypint``.
    """
    return os.path.join(__DATAFILES_DIR__, name)

__all__ = [
    "CFG",
    "data_file",
    "output_dir",
    "new_output_file",
    "remove_output_files",
    "IN_IPYTHON",
]

