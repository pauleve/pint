
from io import StringIO
import json
import os
import subprocess
import tempfile

import networkx as nx
import pydotplus

from .cfg import *
from .types import *
from .ui import *

if IN_IPYTHON:
    from IPython.display import display, FileLink

VALID_EXE = [
    "pint-export",
    "pint-its",
    "pint-lcg",
    "pint-mole",
    "pint-nusmv",
    "pint-reach",
    "pint-sg",
    "pint-stable",
]

class PintProcessError(subprocess.CalledProcessError):
    def __str__(self):
        stderr = "\n%s" % self.stderr.decode() if self.stderr else None
        return "Command '%s' returned non-zero exit status %d%s" \
            % (" ".join(self.cmd), self.returncode, stderr)

def _run_tool(cmd, *args, input_model=None, reduce_for_goal=None, **run_opts):
    assert cmd in VALID_EXE
    args = list(args)
    args.insert(0, "--json-stdout")
    if "stdout" not in run_opts:
        run_opts["stdout"] = subprocess.PIPE
    if "stderr" not in run_opts:
        run_opts["stderr"] = subprocess.PIPE
    if "check" not in run_opts:
        run_opts["check"] = True

    assert (not reduce_for_goal or input_model)

    args.insert(0, cmd)
    if reduce_for_goal:
        pre_args = ["pint-export", "--reduce-for-goal", reduce_for_goal, "--squeeze"]
        pre_kwargs = {}
        input_model.populate_popen_args(pre_args, pre_kwargs)
        pre_cmd = subprocess.Popen(pre_args, stdout=subprocess.PIPE)
        if "input" in pre_kwargs:
            pre_cmd.stdin.write(pre_kwargs["input"])
            pre_cmd.stdin.close()
        run_opts["stdin"] = pre_cmd.stdout
        dbg("Running command %s | %s" % (" ".join(pre_args), " ".join(args)))

    else:
        if input_model is not None:
            input_model.populate_popen_args(args, run_opts)

        dbg("Running command %s" % (" ".join(args)))
    try:
        return subprocess.run(args, **run_opts)
    except subprocess.CalledProcessError as e:
        # backward compatible 'raise e from None'
        e = PintProcessError(e.returncode, e.cmd, e.output, e.stderr)
        e.__cause__ = None
        raise e


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
    if IN_IPYTHON:
        return FileLink(output)
    return output

@modeltool
def reduce(model, goal, squeeze=True):
    output = new_output_file(ext="an")
    args = ["-o", output,
        "--reduce-for-goal", goal]
    if squeeze:
        args.append("--squeeze")
    _run_tool("pint-export", *args, input_model=model, stdout=None)
    if IN_IPYTHON:
        display(FileLink(output))
    from .model import FileModel
    return FileModel(output)


#
# pint-reach
#

@modeltool
def cutsets(model, ai, maxsize=5, exclude_initial_state=True):
    args = []

    info("This computation is an *under-approximation*: returned cut-sets \
are all valid, but they may be non-minimal, and some cut-sets may be missed.")
    info("Limiting results to cut-sets with at most %s elements. Use `maxsize` argument to change." % maxsize)

    if exclude_initial_state:
        args.append("--no-init-cutsets")
    cp = _run_tool("pint-reach", "--cutsets", str(maxsize), ai, *args,
                input_model=model)
    output = cp.stdout.decode()
    return json.loads(output)

@modeltool
def bifurcations(model, ai, method="ua"):
    assert method in ["exact", "ua", "mole+ua"]
    if method == "exact":
        cmd = "pint-nusmv"
        args = ["--bifurcations"]
    else:
        info("This computation is an *under-approximation*: \
returned transitions are all bifurcation transitions, but some may have been missed. \
Use `method=\"exact\"` for complete identification.")
        cmd = "pint-reach"
        args = ["--bifurcations",
            "--bifurcations-method", method]

    cp = _run_tool(cmd, ai, *args, input_model=model)
    output = cp.stdout.decode()
    return [LocalTransition(*d) for d in json.loads(output)]

@modeltool
def reachability(model, ai, fallback="its"):
    if fallback:
        fallback = fallback.lower()
    assert fallback in ["its", "nusmv", "mole", "none", None]
    if fallback == "none":
        fallback = None
    cp = _run_tool("pint-reach", ai, input_model=model)
    output = cp.stdout.decode()
    output = ternary(json.loads(output))
    if output == Inconc and fallback is not None:
        info("Approximations are inconclusive, fallback to exact model-checking with `%s`" % fallback)
        cp = _run_tool("pint-%s" % fallback, ai, input_model=model,
                        reduce_for_goal=ai)
        output = cp.stdout.decode()
        output = ternary(json.loads(output))
    return output

#TODO requirements


#
# pint-lcg
#

@modeltool
def local_causality_graph(model, kind="full", goal=None):
    assert kind in ["verbose,","trimmed","saturated","worth","full"]
    if kind != "full" and goal is None:
        raise ValueError("goal cannot be None with %s LCG" % kind)
    args = ["-t", kind, "-o", "-"]
    if goal:
        args.append(goal)
    cp = _run_tool("pint-lcg", *args, input_model=model)
    g = pydotplus.graph_from_dot_data(cp.stdout.decode())
    return nx.nx_pydot.from_pydot(g)

@modeltool
def full_lcg(model):
    return local_causality_graph(model, "full")
@modeltool
def simple_lcg(model, goal):
    return local_causality_graph(model, "trimmed", goal)
@modeltool
def worth_lcg(model, goal):
    return local_causality_graph(model, "worth", goal)
@modeltool
def saturated_lcg(model, goal):
    return local_causality_graph(model, "saturated", goal)


#
# pint-sg
#

@modeltool
def count_reachable_states(model):
    cp = _run_tool("pint-sg", "--count-reachable", input_model=model)
    output = cp.stdout.decode()
    return json.loads(output)

@modeltool
def summary(model):
    cp = _run_tool("pint-sg", "--description", input_model=model)
    output = cp.stdout.decode()
    return json.loads(output)

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
    return json.loads(output)

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


