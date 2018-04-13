from io import StringIO
import json
import os
import subprocess
import tempfile

import networkx as nx

from .cfg import *
from .types import *
from .ui import *
from .utils import *

if IN_IPYTHON:
    from IPython.display import FileLink

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
    """
    Exception raised when a Pint command fails.
    """
    def __str__(self):
        stderr = "\n%s" % self.stderr.decode() if self.stderr else self.output.decode()
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

    args = [a.to_pint() if isinstance(a, Goal) else a for a in args]

    args.insert(0, cmd)

    if reduce_for_goal:
        pre_args = ["pint-export", "--reduce-for-goal",
                        reduce_for_goal.to_pint(), "--squeeze"]
        for a in reduce_for_goal.automata:
            pre_args += ["--squeeze-preserve", a]
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
    "asp": "asp",
    "dump": "an",
    "nusmv": "smv",
    "pep": "ll",
    "pnml": "pnml",
    "romeo": "xml",
}
ext2format = dict([(j,i) for (i,j) in format2ext.items()])
ext2format["lp"] = "asp"

EXPORT_SUPPORTED_FORMATS = list(sorted(format2ext.keys()))
"""
Formats supported by :py:meth:`.Model.export` method.
"""

EXPORT_SUPPORTED_EXTENSIONS = list(sorted(ext2format.keys()))
"""
File extensions supported by :py:meth:`.Model.save_as` method:

* ``.an``: Pint native format
* ``.ll``: PEP format (Petri net)
* ``.pnml``: PNML format (Petri net)
* ``.smv``: NuSMV format
* ``.xml``: Romeo format (Petri net)
"""

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
def export(self, format, output=None, raw_args=None):
    """
    Export the AN model to the given `format`. If not provided the `output`
    filename is generated using :py:func:`.new_output_file` and the extension
    corresponding to the export format.

    .. seealso:: :py:data:`.EXPORT_SUPPORTED_FORMATS`, and similar method :py:meth:`.save_as`
    """
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
                input_model=self, stdout=None)
    if IN_IPYTHON:
        return FileLink(output)
    return output

@modeltool
def save_as(self, filename):
    """
    Save the AN model with its initial state to local file `filename`.
    The format is guessed from the extension of `filename`.

    .. seealso:: :py:data:`.EXPORT_SUPPORTED_EXTENSIONS`, and similar method :py:meth:`.export`

    Examples:

    >>> m.save_as("mymodel.an") # save to a local file in Pint native format
    >>> m.save_as("mymodel.ll") # export in PEP format (Petri net)
    """
    ext = file_ext(filename)
    assert ext in ext2format, "File extension is not supported"
    return export(self, ext2format[ext], output=filename)

@modeltool
def to_nusmv(self, skip_init=True):
    """
    TODO

    :keyword bool skip_init: do not generate an INIT statement.
    """
    format = "nusmv"
    smvfile = new_output_file(ext=format2ext[format])
    mapfile = new_output_file(ext="json")

    raw_args = ["--mapfile", mapfile]
    if skip_init:
        raw_args += ["--no-init"]
    try:
        export(self, format, output=smvfile, raw_args=raw_args)
        with open(mapfile) as mf:
            bindings = json.load(mf)
    finally:
        os.unlink(mapfile)

    bindings = dict([(tuple(k),v) for k,v in bindings])
    from colomoto.modelchecking import ColomotoNuSMV
    return ColomotoNuSMV(smvfile, bindings.get)


def _model_modification(self, args):
    output = new_output_file(ext="an")
    args += ["-o", output]
    _run_tool("pint-export", *args, input_model=self, stdout=None)
    from .model import FileModel
    return FileModel(output)


@modeltool
def simplify(self):
    """
    Simplify the local transitions by computing their prime implicants.

    :returns: a new :py:class:`.FileModel` instance for the reduced automata
        network.
    """
    return _model_modification(self, ["--simplify"])

@modeltool
def reduce_for_goal(self, goal=None, squeeze=True, squeeze_preserve=None, **kwgoal):
    """
    Compute the goal-oriented reduction of the automata network.
    The reduction removes local transitions that are guaranteed to never
    contribute to any minimal path from the initial state to the goal
    (a path is minimal if there exists no paths using a strict subset of
    transitions leading to the goal).

    The reduced models preserves existential reachability properties, as well as
    cut sets.

    The complexity of the method is polynomial in the number of local
    transitions and automata, and exponential in the size of the largest
    automata minus 1.

    :param goal: goal specification (e.g., ``"a=1"``)
    :type goal: str or list(str) or .Goal
    :keyword kwgoal: keywords for goal specification (instead of `goal` argument)
    :param bool squeeze: if ``True`` (default), unused automata and local states
        are removed. Warning: this can lead to local state renaming.
    :param str list squeeze_preserve:
        do not squeeze the given automata
    :returns: a new :py:class:`.FileModel` instance for the reduced automata
        network.

    Example:

    >>> redm = m.reduce_for_goal("g=1")
    """
    goal = Goal.from_arg(goal, **kwgoal)

    args = ["--reduce-for-goal", goal.to_pint()]
    if squeeze:
        args.append("--squeeze")
        if squeeze_preserve:
            for a in squeeze_preserve:
                args += ["--squeeze-preserve", a]
    fm = _model_modification(self, args)
    return fm

@modeltool
def lock(self, substate={}, **kwstate):
    """
    Returns the AN where the specified automata are lock to the given local state.
    The initial state is modified accordingly.

    :param substate: local states to lock.
    :type substate: list(tuple(str,int)) or dict[str,int]
    :keyword kwstate: complementary specification of local states.
    :returns: a new :py:class:`.FileModel` instance for the modified AN.
    """
    lockstate = dict(**kwstate)
    if hasattr(substate, "items"):
        substate = substate.items()
    for a,i in substate:
        assert (isinstance(i, (str, int)) and \
            (not a in lockstate or lockstate[a] == i)), \
            "Cannot lock an automaton to different local states"
        lockstate[a] = i

    args = ["--lock", pint_of_localstates(lockstate.items())]

    return _model_modification(self, args)


@modeltool
def disable(self, local_states=[], **kwstate):
    """
    Returns the AN where all local transitions involving specified local states
    have been removed.

    :param local_states: local states to disable.
    :type local_states: list(tuple(str,int)) or dict[str,int]
    :keyword kwstate: complementary specification of local states.
    :returns: a new :py:class:`.FileModel` instance for the modified AN.
    """
    if hasattr(local_states, "items"):
        local_states = list(local_states.items())
    else:
        local_states = list(local_states)
    local_states += list(kwstate.items())

    args = ["--disable", pint_of_localstates(local_states)]

    return _model_modification(self, args)


#
# pint-reach
#

@modeltool
def cutsets(self, goal=None, maxsize=5, exclude=[], exclude_initial_state=False,
                exclude_goal_automata=True, timeout=None, **kwgoal):
    """
    Computes sets of local states which are used in all the paths from the
    initial state to `goal`:
    if all the local states in a cut-set are disabled (all related local
    transitions are removed), then it is impossible to reach `goal` from the
    initial state.

    Elements of the returned lists can be notably used as argument for methods
    :py:meth:`.Model.having` and :py:meth:`.disable`.

    :param goal: goal specification (e.g., ``"a=1"``)
    :type goal: str or list(str) or .Goal
    :keyword kwgoal: keywords for goal specification (instead of `goal` argument)
    :keyword int maxsize: maximal cardinality of a cut-set.
    :keyword set(str) exclude:
        set/list of automata to exclude from the solutions
    :keyword bool exclude_initial_state:
        if ``True``, cut-sets can not be composed of initial local states.
    :keyword bool exclude_goal_automata:
        exclude automata involved in the goal specification
    :param int timeout: command timeout in seconds
    :rtype: list(dict[str,int or int list])

    .. seealso:: method :py:meth:`.oneshot_mutations_for_cut`
    """
    goal = Goal.from_arg(goal, **kwgoal)

    args = []

    info("This computation is an *under-approximation*: returned cut-sets \
are all valid, but they may be non-minimal, and some cut-sets may be missed.")
    info("Limiting results to cut-sets with at most %s elements. Use `maxsize` argument to change." % maxsize)

    if exclude_initial_state:
        args.append("--no-init-cutsets")

    if isinstance(exclude, str):
        exclude = set([exclude])
    else:
        exclude = set(exclude)
    if exclude_goal_automata:
        exclude.update(goal.automata)
    if exclude:
        args += ["--ignore-automata", ",".join(['"%s"' % a for a in exclude])]

    cp = _run_tool("pint-reach", "--cutsets", str(maxsize), goal, *args,
                input_model=self, timeout=timeout)
    output = cp.stdout.decode()
    return json.loads(output)

@modeltool
def oneshot_mutations_for_cut(self, goal=None, maxsize=5,
        exclude=[],
        exclude_goal_automata=True,
        timeout=None,
        **kwgoal):
    """
    Computes sets of local states for which the locking in the initial state
    ensures that `goal` is impossible to reach.

    Elements of the returned lists can be notably used as argument for methods
    :py:meth:`.Model.having` and :py:meth:`.lock`.

    :param goal: goal specification (e.g., ``"a=1"``)
    :type goal: str or list(str) or .Goal
    :keyword kwgoal: keywords for goal specification (instead of `goal` argument)
    :keyword int maxsize: maximal cardinality of returned sets.
    :keyword set(str) exclude:
        set/list of automata to exclude from the solutions
    :keyword bool exclude_goal_automata:
        exclude automata involved in the goal specification
    :param int timeout: command timeout in seconds
    :rtype: list(dict[str,int])
    """
    goal = Goal.from_arg(goal, **kwgoal)

    info("This computation is an *under-approximation*: returned mutations \
are all valid, but they may be non-minimal, and some solutions may be missed.")
    info("Limiting solutions to mutations of at most %s automata. Use `maxsize` argument to change." % maxsize)

    args = ["--oneshot-mutations-for-cut", str(maxsize)]

    if isinstance(exclude, str):
        exclude = set([exclude])
    else:
        exclude = set(exclude)
    if exclude_goal_automata:
        exclude.update(goal.automata)
    if exclude:
        args += ["--ignore-automata", ",".join(['"%s"' % a for a in exclude])]

    cp = _run_tool("pint-reach", goal, *args, input_model=self, timeout=timeout)
    output = cp.stdout.decode()
    return json.loads(output)

@modeltool
def bifurcations(self, goal=None, method="ua", timeout=None, **kwgoal):
    """
    Identify local transitions after which, in some state, `goal` is no longer reachable.

    :param goal: goal specification (e.g., ``"a=1"``)
    :type goal: str or list(str) or .Goal
    :keyword kwgoal: keywords for goal specification (instead of `goal` argument)
    :keyword str method:

        * ``"exact"`` for complete identification of bifurcation transitions
          (PSPACE);
        * ``"ua+mole"`` for under-approximation relying on exact the reachable
          states set prior computation (NP+PSPACE)
        * ``"ua"`` for under-approximation of bifurcation transitions (NP)
    :param int timeout: command timeout in seconds
    :rtype: :py:class:`.LocalTransition` list
    """
    assert method in ["exact", "ua", "mole+ua"]

    goal = Goal.from_arg(goal, **kwgoal)

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

    cp = _run_tool(cmd, goal, *args, input_model=self, timeout=timeout)
    output = cp.stdout.decode()
    return [local_transition_from_json(d) for d in json.loads(output)]


__reachability_tools = ["its", "nusmv", "mole"]

@modeltool
def reachability(self, goal=None, fallback="its", tool="sa",
        sa_args=[],
        reduce_for_goal=True, timeout=None, **kwgoal):
    """
    Check if `goal` is reachable from the initial state.
    At first, Pint tries static analysis for the verification. If
    non-conclusive, it can fallback to exact model-checking.

    :param goal: goal specification (e.g., ``"a=1"``)
    :type goal: str or list(str) or .Goal
    :keyword kwgoal: keywords for goal specification (instead of `goal` argument)
    :keyword str fallback: fallback to exact model-checking if static analysis
        is not conclusive. Supported model-checkers are: ``"its"``, ``"nusmv"``,
        and ``"mole"``.
    :keyword str tool: tool for the model-checking:
        * ``"sa"``: static analysis with potential fallback method if not
        conclusive
        * ``"its"``, ``"nusmv"``, ``"its"``: directly use the specified
        model-checker.
    :keyword list(str) sa_args: additional arguments for static analysis
    :keyword bool reduce_for_goal: before invoking a model-checker, perform the
        goal-oriented reduction of the automata network
    :param int timeout: command timeout in seconds
    :returns:

        * ``True`` if `goal` is reachable from :py:attr:`.initial_state`
        * ``False`` if `goal` is not reachable from :py:attr:`.initial_state`
        * :py:class:`.Inconc` if the static analysis is not conclusive and
          `fallback` is ``None``.
    """
    goal = Goal.from_arg(goal, **kwgoal)

    if fallback:
        fallback = fallback.lower()
        if fallback == "none":
            fallback = None
    tool = tool.lower()
    assert fallback in __reachability_tools + [None]
    assert tool in ["sa"] + __reachability_tools
    if tool == "sa":
        cp = _run_tool("pint-reach", goal, *sa_args, input_model=self, timeout=None)
        output = cp.stdout.decode()
        output = ternary(json.loads(output))
        if output == Inconc and fallback is not None:
            info("Approximations are inconclusive, fallback to exact model-checking with `%s`" % fallback)
            tool = fallback
    if tool != "sa":
        cp = _run_tool("pint-%s" % tool, goal, input_model=self,
                        reduce_for_goal=goal if reduce_for_goal else None,
                        timeout=timeout)
        output = cp.stdout.decode()
        output = ternary(json.loads(output))
    return output

#TODO requirements


#
# pint-lcg
#

@modeltool
def local_causality_graph(self, kind="full", goal=None, **kwgoal):
    """
    Computes the Local Causality Graph (LCG) of type `kind`.

    :keyword str kind:

        * ``"full"``: LCG for all possible objectives from all possible initial
          states.
        * ``"verbose"``: LCG for simple over-approximation of `goal` rechability
        * ``"trimmed"``: LCG for simple over-approximation of `goal`
          reachability where impossible objectives have been removed.
        * ``"saturated"``: LCG for under-approximation of `goal` reachability
        * ``"worth"``: LCG used for `goal`-oriented reduction (see
          :py:meth:`.reduce`)

    :param goal: goal specification (e.g., ``"a=1"``) when `kind` is not ``"full"``.
    :type goal: str or list(str) or .Goal
    :keyword kwgoal: keywords for goal specification (instead of `goal` argument)
    :rtype: NetworkX multigraph (`nx.MultiDiGraph <http://networkx.readthedocs.io/en/stable/reference/classes.multidigraph.html>`_)

    .. seealso: methods :py:meth:`.full_lcg`, :py:meth:`.simple_lcg`, :py:meth:`.worth_lcg`, :py:meth:`.saturated_lcg`
    """
    assert kind in ["verbose","trimmed","saturated","worth","full"]
    goal = Goal.from_arg(goal, **kwgoal) if kind != "full" else None
    args = ["-t", kind, "-o", "-"]
    if goal:
        args.append(goal)
    cp = _run_tool("pint-lcg", *args, input_model=self)
    dot = StringIO(cp.stdout.decode())
    return nx.DiGraph(nx.nx_pydot.read_dot(dot))

@modeltool
def full_lcg(self):
    """
    Shortcut for :py:meth:`.local_causality_graph` with `kind="full"`
    """
    return local_causality_graph(self, "full")
@modeltool
def simple_lcg(self, goal=None, **kwgoal):
    """
    Shortcut for :py:meth:`.local_causality_graph` with `kind="trimmed"`
    """
    return local_causality_graph(self, "trimmed", goal=goal, **kwgoal)
@modeltool
def worth_lcg(self, goal, **kwgoal):
    """
    Shortcut for :py:meth:`.local_causality_graph` with `kind="worth"`
    """
    return local_causality_graph(self, "worth", goal=goal, **kwgoal)
@modeltool
def saturated_lcg(self, goal, **kwgoal):
    """
    Shortcut for :py:meth:`.local_causality_graph` with `kind="saturated"`
    """
    return local_causality_graph(self, "saturated", goal=goal, **kwgoal)


#
# pint-sg
#

@modeltool
def count_reachable_states(self, tool="pint", timeout=None):
    """
    Counts the exact number of states reachable from :py:attr:`.initial_state`.
    Uses an explicit state space approach.

    :keyword str tool:

        * ``"pint"`` explicit reachable state space computation (default);
        * ``"its"`` symbolic reable stae space computation
    :param int timeout: command timeout in seconds
    :rtype: int
    """
    assert tool in ["pint", "its"], "Wrong `tool` argument. See help."
    if tool == "pint":
        argv = ["pint-sg", "--count-reachable"]
    else:
        argv = ["pint-its", "--tool", "count"]
    cp = _run_tool(*argv, input_model=self, timeout=timeout)
    output = cp.stdout.decode()
    return json.loads(output)

@modeltool
def summary(model):
    """
    Returns a dictionnary with various information on the AN model:

    * ``"nb_automata"``: number of automata (equivalent to `len(m.automata)`)
    * ``"nb_local_states"``: total number of local states
    * ``"max_local_states"``: largest number of local states within one automaton.
    * ``"nb_transitions"``: number of local transitions
    * ``"nb_states"``: total number of states

    :rtype: dict
    """
    cp = _run_tool("pint-sg", "--description", input_model=model)
    output = cp.stdout.decode()
    return json.loads(output)

@modeltool
def reachable_states(self, timeout=None):
    """
    Returns the list of states reachable from :py:attr:`.initial_state`.

    :param int timeout: command timeout in seconds
    :rtype: state list
    """
    cp = _run_tool("pint-sg", "--reachable-states", input_model=self,
            timeout=timeout)
    output = cp.stdout.decode()
    return json.loads(output)

@modeltool
def reachable_stategraph(self, timeout=None):
    """
    Returns the reachable state graph from :py:attr:`.initial_state`.

    :param int timeout: command timeout in seconds
    :rtype: NetworkX digraph (`nx.DiGraph <http://networkx.readthedocs.io/en/stable/reference/classes.digraph.html>`_)
    """
    dotfile = new_output_file(ext="dot")
    _run_tool("pint-sg", "--state-graph", dotfile,
                input_model=self, stdout=None, timeout=timeout)
    g = nx.DiGraph(nx.nx_agraph.read_dot(dotfile))
    os.unlink(dotfile)
    return g

@modeltool
def reachable_attractors(self, timeout=None):
    """
    Returns the complete list of attractors reachable from
    :py:attr:`.initial_state`.

    Uses an explicit state space exploration methods.

    :param int timeout: command timeout in seconds

    Each attractor is described by a `dict` object with the following keys:

    * ``"type"``: either ``"fixpoint"`` or ``"cyclic"``.
    * ``"size"``: number of states in the attractor (1 if fixpoint, >1 if
      cyclic).
    * ``"sample"``: state (represented as `dict`) belonging to the attractor,
      i.e., either the fixpoint, or one of the state in the cycle attractor.
    """
    cp = _run_tool("pint-sg", "--reachable-attractors", input_model=self,
            timeout=timeout)
    output = cp.stdout.decode()
    return json.loads(output)

#
# pint-stable
#

@modeltool
def fixpoints(self, timeout=None):
    """
    Returns the complete list of fixed points of the model.

    :param int timeout: command timeout in seconds
    :rtype: dict list
    """
    cp = _run_tool("pint-stable", "--fixpoints", input_model=self,
            timeout=timeout)
    output = cp.stdout.decode()
    return json.loads(output)


#
# misc
#
@modeltool
def dependency_graph(self):
    """
    Returns the dependency graph between automata:
    there is an edge from `a` to `b` if some local transitions of `b` depends on `a`.

    Complexity: linear with the number of local transitions.

    :rtype: NetworkX digraph (`nx.DiGraph <http://networkx.readthedocs.io/en/stable/reference/classes.digraph.html>`_)
    """
    g = nx.DiGraph()
    g.add_nodes_from(self.automata)
    for tr in self.local_transitions:
        for a in tr.modified_automata:
            for b in tr.conds.keys():
                g.add_edge(b, a)
    return g


@modeltool
def automaton_graph(self, a):
    """
    Returns the directed graph of local transitions between the local states of
    automaton `a`. Edges are labeled with the index of the transitions in
    :py:attr:`.local_transitions`.

    :rtype: NetworkX digraph (`nx.DiGraph <http://networkx.readthedocs.io/en/stable/reference/classes.digraph.html>`_)
    """
    g = nx.DiGraph()
    g.add_nodes_from(self.local_states[a])
    for (idx, tr) in enumerate(self.local_transitions):
        if a in tr.modified_automata:
            if isinstance(tr, LocalTransition):
                i, j = tr.i, tr.j
            else:
               ((i, j),) = [(i,j) for (b,i,j) in tr.local_transitions if b == a]
            g.add_edge(i, j, label=str(idx))
    return g


__all__ = [t[0] for t in __MODEL_TOOLS] + [
    "EXPORT_SUPPORTED_FORMATS",
    "EXPORT_SUPPORTED_EXTENSIONS",
    "PintProcessError",
    "EquipTools",
    ]


