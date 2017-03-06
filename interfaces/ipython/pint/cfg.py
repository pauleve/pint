
import os
import tempfile

CFG = {
    "output_dir": "gen",
    "dbg": False,
}

try:
    get_ipython()
    IN_IPYTHON = True
except NameError:
    IN_IPYTHON = False

def output_dir():
    if not os.path.exists(CFG["output_dir"]):
        os.makedirs(CFG["output_dir"])
    return CFG["output_dir"]


def new_output_file(ext=None, **tempargs):
    if "prefix" not in tempargs:
        tempargs["prefix"] = "pint"
    if ext is not None:
        tempargs["suffix"] = "%s.%s" % (tempargs.get("suffix", ""), ext)
    _, filename = tempfile.mkstemp(dir=output_dir(), **tempargs)
    return os.path.relpath(filename)

__all__ = [
    "output_dir",
    "new_output_file",
    "IN_IPYTHON",
    "CFG",
]

