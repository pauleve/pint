import re
import sys

from xml.dom.minidom import parse

from pypint.cfg import CFG

"""
Unsupported (TODO):
- MacroNodes
- transition/action
- transition/event
- transition/fact_ids
- transition/CDATA
"""

def import_cadbiom(localfile, outfd, full_context=True, relax_threshold=6):
    try:
        import boolean
    except ImportError:
        print("ERROR: Please install boolean.py package (pip3 install boolean.py)")
        raise
    from .lib.boolean_utils import BoolToAN

    ba = boolean.BooleanAlgebra()

    bcx = parse(localfile)

    trMap = {}
    def tr(name):
        return trMap.get(name, name)

    def register_name(name) :
        if ord("0") <= ord(name[0]) <= ord("9"):
            new_name = "v%s" % name
            trMap[name] = new_name
            return new_name
        return name

    # fetch nodes
    SimpleNodes = []
    for nt in ["CSimpleNode", "CTrapNode", "CMacroNode", "CStartNode"]:
        SimpleNodes += [register_name(n.getAttribute("name")) for n in bcx.getElementsByTagName(nt)]
    PermNodes = [register_name(n.getAttribute("name")) for n in bcx.getElementsByTagName("CPermNode")]

    def declare_automaton(a, *states):
        print("%s [%s]" % (a, ",".join(map(str,states))), file=outfd)

    # declare automata
    for a in SimpleNodes + PermNodes:
        declare_automaton(a, 0, 1)

    def ls_of_lit(lit):
        if isinstance(lit, ba.NOT):
            return (lit.args[0].obj, 0)
        else:
            return (lit.obj, 1)
    def out(data):
        print(data, file=outfd)
    b2a = BoolToAN(ba, ls_of_lit, out)

    def relabel_condition(condition):
        def relabel(m) :
            name = m.group(1)
            if name not in ["and", "or", "not"]:
                return tr(name)
            return name
        return re.sub(r"\b(\w+)\b", relabel, condition)

    transitions = bcx.getElementsByTagName("transition")

    bar = None
    if CFG["console"]:
        try:
            from progress.bar import Bar
            bar = Bar("converting", max=len(transitions))
        except ImportError:
            pass

    done = {}
    non_frontier = set()

    acoop = "__pint_coop%d"
    ncoop = 0

    for t in transitions:
        orig = tr(t.getAttribute("ori"))
        target = tr(t.getAttribute("ext"))
        condition = relabel_condition(t.getAttribute("condition")) or "1"

        non_frontier.add(target)

        lorig, ltarget = ba.symbols(orig, target)

        choices = []
        condition = ba.dnf(ba.parse(condition))
        if condition == ba.TRUE:
            expr_up = lorig
        else:
            if isinstance(condition, ba.OR):
                choices = [node for node in condition.args \
                    if isinstance(node, ba.AND)]
            expr_up = condition & lorig
            expr_up = expr_up.subs({ltarget: ba.FALSE})
            expr_up = ba.dnf(expr_up)

        # check duplicates
        if target in done:
            isdup = False
            for e in done[target]:
                if e == expr_up:
                    isdup = True
                    break
            if isdup:
                if bar:
                    bar.next()
                continue
            done[target].append(expr_up)
        else:
            done[target] = [expr_up]

        b2a.make_transitions([(target, 0, 1)], expr_up)

        if condition != ba.TRUE:
            condition = condition.subs({ltarget: ba.TRUE}).simplify()

        if condition == ba.TRUE:
            expr_down = ~lorig
        else:
            if len(choices) >= relax_threshold:
                for clause in choices:
                    ncoop += 1
                    a = acoop % ncoop
                    declare_automaton(a, 0, 1)
                    b2a.make_transitions([(a, 0, 1)], clause)
                    b2a.make_transitions([(a, 1, 0)], ~clause)
                    (la,) = ba.symbols(a)
                    condition = condition.subs({clause:la})

            expr_down = ~condition | ~lorig

        b2a.make_transitions([(target, 1, 0)], expr_down)

        if bar:
            bar.next()
    if bar:
        bar.finish()

    init_procs = ["%s=1" % a for a in PermNodes]
    if full_context:
        # add frontiers
        nodes = set(SimpleNodes)
        nodes.difference_update(non_frontier)
        init_procs += ["%(sort)s=0, %(sort)s=1" %{"sort": n} for n in nodes]
        bar = None
    print("initial_context %s" % ", ".join(init_procs), file=outfd)

