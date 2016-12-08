
import subprocess

from IPython.display import FileLink

import tempfile

from .cfg import *
from .ui import *

VALID_EXE = ["pint-export", "pint-reach"]

def _run_tool(cmd, *args, input_model=None, **run_opts):
    assert cmd in VALID_EXE
    args = list(args)

    if "stdout" not in run_opts:
        run_opts["stdout"] = subprocess.PIPE
    if "check" not in run_opts:
        run_opts["check"] = True

    if input_model is not None:
        input_model.populate_popen_args(args, run_opts)

    dbg("Running command %s %s" % (cmd, " ".join(args)))
    return subprocess.run([cmd]+args, **run_opts)


format_alias = {
    "an": "dump",
}

format2ext = {
    "dump": "an",
    "nusmv": "smv",
    "pep": "ll",
    "romeo": "xml",
}

def export(model, format, *raw_args):
    format = format.lower()
    format = format_alias.get(format, format)
    assert format in format2ext
    assert "-o" not in raw_args
    output = new_output_file(ext=format2ext[format])
    _run_tool("pint-export", "-l", format, "-o", output, *raw_args,
                input_model=model, stdout=None)
    return FileLink(output)

def cutsets(model, ai, maxsize=5, *args, exclude_initial_state=True, **opts):
    args = list(args)
    if exclude_initial_state:
        args.append("--no-init-cutsets")
    cp = _run_tool("pint-reach", "--cutsets", str(maxsize), ai, *args,
                input_model=model, **opts)
    output = cp.stdout.decode()
    if " is not reachable.\n" in output:
        print("Goal is not reachable, nothing to cut!")
    else:
        return output.split()

MODEL_TOOLS = [
    ("export", export),
    ("cutsets", cutsets),
]

def EquipTools(cls):
    for name, func in MODEL_TOOLS:
        setattr(cls, name, func)
    return cls

__all__ = [t[0] for t in MODEL_TOOLS] + [
    "MODEL_TOOLS",
    "EquipTools",
    ]


