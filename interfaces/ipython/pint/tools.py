
import json
import os
import subprocess
import tempfile

from IPython.display import FileLink

import networkx as nx

from .cfg import *
from .ui import *

VALID_EXE = ["pint-export", "ping-lcg", "pint-reach", "pint-sg",
                "pint-stable"]

def _run_tool(cmd, *args, input_model=None, **run_opts):
    assert cmd in VALID_EXE
    args = list(args)
    args.insert(0, "--json-stdout")
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

__MODEL_TOOLS = []
def modeltool(f):
    __MODEL_TOOLS.append((f.__name__, f))
    return f

def EquipTools(cls):
    for name, func in __MODEL_TOOLS:
        setattr(cls, name, func)
    return cls

#
# pint-export
#

@modeltool
def export(model, format, output=None, raw_args=None):
    format = format.lower()
    format = format_alias.get(format, format)
    assert format in format2ext
    args = ["-l", format]
    if raw_args is None:
        raw_args = []
    assert "-o" not in raw_args
    if not output:
        output = new_output_file(ext=format2ext[format])
    args += ["-o", output]
    _run_tool("pint-export", *args, *raw_args,
                input_model=model, stdout=None)
    return FileLink(output)


#
# pint-reach
#

@modeltool
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

@modeltool
def bifurcations(model, ai, method="ua"):
    assert method in ["ua", "mole+ua"]
    args = ["--bifurcations",
        "--bifurcations-method", method]
    cp = _run_tool("pint-reach", ai, *args, input_model=model)
    output = cp.stdout.decode()
    print(output)
    # TODO: parse output

@modeltool
def reachability(model, ai, fallback="its"):
    assert fallback in ["its", "nusmv", "none"]
    cp = _run_tool("pint-reach", ai, input_model=model)
    output = cp.stdout.decode().strip()
    if output == "True":
        return True
    elif output == "False":
        return False
    elif fallback == "none":
        return "Inconclusive"
    else:
        info("pint is inconclusive, fallback to %s" % fallback)
        # TODO: model reduction
        cp = _run_tool("pint-%s" % fallabck, ai, input_model=model)
        return cp.stdout.decode().strip()

#
# pint-sg
#

@modeltool
def count_reachable_states(model):
    cp = _run_tool("pint-sg", "--count-reachable", input_model=model)
    output = cp.stdout.decode()
    return int(output.split()[0])

@modeltool
def reachable_stategraph(model):
    dotfile = new_output_file(ext="dot")
    _run_tool("pint-sg", "--state-graph", dotfile,
                input_model=model, stdout=None)
    g = nx.nx_agraph.read_dot(dotfile)
    os.unlink(dotfile)
    return g

@modeltool
def reachable_attractors(model):
    cp = _run_tool("pint-sg", "--reachable-attractors", input_model=model)
    output = cp.stdout.decode()
    print(output) # TODO: parse output

#
# pint-stable
#

@modeltool
def fixpoints(model):
    cp = _run_tool("pint-stable", "--fixpoints", input_model=model)
    output = cp.stdout.decode()
    return json.loads(output)


__all__ = [t[0] for t in __MODEL_TOOLS] + [
    "EquipTools",
    ]


