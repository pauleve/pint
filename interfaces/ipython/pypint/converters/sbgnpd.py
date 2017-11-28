
from .lib.sbgnpd import *

def import_sbgnpd(sbgnpd_filename, outfd, initial_state=(), stories=()):
    from .lib.boolean_utils import BoolToAN

    ba = BoolToAN.BooleanAlgebra()

    model = parse_sbgnpd(sbgnpd_filename)
    def resolve_ids(ids):
        return [model.resolve_clone(model.entities[eid]) for eid in ids]

    def _p(name):
        return '"{}"'.format(name)

    def out(data):
        print(data, file=outfd)

    # format stories specification
    stories_spec = []
    for i, story in enumerate(stories):
        if isinstance(story, tuple) and len(story) == 2 \
                and isinstance(story[0], str) \
                and isinstance(story[1], (tuple, list, set)):
            spec = {"name": _p(story[0]), "ids": tuple(story[1])}
        else:
            spec = {"name": _p("story{}".format(i)), "ids": story}
        spec["entities"] = resolve_ids(spec["ids"])
        stories_spec.append(spec)
    stories = stories_spec

    e2stories = {}
    for i, story in enumerate(stories):
        for e in story["entities"]:
            if e not in e2stories:
                e2stories[e] = [i]
            else:
                e2stories[e].append(i)

    def an_name(e):
        return _p(e.name)

    # declare conflicts
    conflicts = {}
    for story in stories:
        procs = set()
        for e in story["entities"]:
            procs.update(e.consumers)
        for p in procs:
            if p not in conflicts:
                conflicts[p] = set()
            conflicts[p].update(procs)

    # filter self-conflicts
    for p in conflicts:
        if p in conflicts[p]:
            conflicts[p].remove(p)

    # declare automata for entities
    # TODO? clones
    entities = set([e for e in model.entities.values() if not e.is_void()])

    name_clash = {}
    for e in entities:
        name = an_name(e)
        assert name not in name_clash, \
            "Name clash [%s] between %s and %s" % (name, e, name_clash[name])
        name_clash[name] = e

    for e in entities:
        if e not in e2stories:
            out("{} [0, 1]".format(an_name(e)))
    for story in stories:
        out("{} [{}]".format(story["name"],
                        ", ".join(["0"] + [an_name(e) for e in story["entities"]])))

    def Lit(e):
        return ba.symbols(e)[0]

    def logic_from_entity(n):
        if n.type in ENTITY_CLASSES:
            return Lit(n)
        elif n.type == "and":
            expr = logic_from_entity(n.inputs[0])
            for m in n.inputs[1:]:
                expr &= logic_from_entity(m)
            return expr
        elif n.type == "or":
            expr = logic_from_entity(n.inputs[0])
            for m in n.inputs[1:]:
                expr |= logic_from_entity(m)
            return expr

    def logic_of_modulations(modulations):
        expr = ba.FALSE
        necessary = ba.TRUE
        for (cls, var) in modulations:
            varexpr = logic_from_entity(var)
            if cls == "necessary stimulation":
                necessary &= varexpr
            elif cls in ["stimulation", "catalysis","modulation"]:
                expr |= varexpr
            elif cls in ["inhibition", "modulation"]:
                expr |= ~varexpr
        if expr is ba.FALSE:
            expr = ba.TRUE
        return necessary & expr

    def story_name(sid):
        return stories[sid]["name"]

    def local_states_of_entity(e, present=True):
        sids = e2stories.get(e)
        if sids:
            ret = []
            for sid in sids:
                a = story_name(sid)
                if present:
                    ret.append((a,an_name(e)))
                else:
                    ret += [(a,an_name(f)) for f in stories[sid] if f.id != e.id] \
                        + [(a, 0)]
            return ret
        else:
            return [(an_name(e), 1 if present else 0)]

    def cond_of_lit(b, pos):
        if isinstance(b, tuple): # story,0
            return "%s=%s" % b
        else:
            return " and ".join(["{}={}".format(a,i) \
                        for (a,i) in local_states_of_entity(b, pos)])
        return b, 1 if pos else 0

    out_trs = []

    b2a = BoolToAN(ba, cond_of_lit, out_trs.append)

    #
    # transitions + automata for explicit processes
    #
    for p in model.processes.values():
        conds = logic_of_modulations(p.modulations)
        pconflicts = conflicts.get(p, ())

        if not pconflicts and len(p.consumptions) == 0:
            for e in p.productions:
                for a,j in local_states_of_entity(e):
                    b2a.make_transitions([(a,0,j)], conds)
        elif not pconflicts and len(p.productions) == 0 and len(p.consumptions) == 1:
            e = p.consumptions[0]
            trs = [(a,i,0) for a,i in local_states_of_entity(e)]
            b2a.make_transitions(trs, conds)
        else:

            implicit_process = not pconflicts and \
                                len(p.consumptions) <= 1 and \
                                len(p.productions) <= 1
            if implicit_process:
                for e in p.consumptions + p.productions:
                    if e not in e2stories:
                        implicit_process = False
                        break

            if not implicit_process:
                out("{} [0,1]".format(an_name(p)))
                for e in p.consumptions:
                    conds &= Lit(e)
                for c in pconflicts:
                    conds &= ~Lit(c)
                b2a.make_transitions([(an_name(p),0,1)], conds)
                conds = Lit(p)

            gstories = {}
            def push_sids(sids):
                for sid1 in sids:
                    for sid2 in sids:
                        if sid2 not in gstories:
                            gstories[sid1] = set()
                        gstories[sid1].add(sid2)
            def fill_group(group, sid):
                if sid in group:
                    return
                group.add(sid)
                sids = gstories[sid]
                for sid in sids:
                    fill_group(group, sid)

            consumptions = p.consumptions
            productions = p.productions

            inv = set(p.productions).intersection(p.consumptions)
            if inv:
                consumptions = set(consumptions) - inv
                productions = set(productions) - inv

            cons_indiv_e = []
            cons_s2e = {}
            prod_s2e = {}
            prod_done = ba.TRUE
            for e in consumptions:
                sids = e2stories.get(e)
                if not sids:
                    cons_indiv_e.append(e)
                else:
                    push_sids(sids)
                    for sid in sids:
                        cons_s2e[sid] = e
            for e in productions:
                sids = e2stories.get(e)
                if not sids:
                    b2a.make_transitions([(an_name(e),0,1)], conds)
                else:
                    push_sids(sids)
                    for sid in sids:
                        prod_s2e[sid] = e
                prod_done &= Lit(e)

            known = set()
            groups = []
            for sid in gstories:
                if sid in known:
                    continue
                group = set()
                fill_group(group, sid)
                groups.append(group)
                known.update(group)

            for sids in groups:
                changes = []
                for sid in sids:
                    a = story_name(sid)
                    i = cons_s2e.get(sid)
                    j = prod_s2e.get(sid)
                    i = an_name(i) if i is not None else 0
                    j = an_name(j) if j is not None else 0
                    changes.append((a,i,j))
                    if not j:
                        production_done &= Lit((sid,0))
                b2a.make_transitions(changes, conds)

            # process de-activation
            if not implicit_process:
                done = prod_done
                b2a.make_transitions([(an_name(p), 1, 0)], done)
            else:
                done = ba.TRUE

            for e in cons_indiv_e:
                b2a.make_transitions([(an_name(e),1,0)], conds & done)

    for spec in sorted(out_trs):
        out(spec)

    if initial_state:
        init = []
        initial_state = resolve_ids(initial_state)
        for e in initial_state:
            init += local_states_of_entity(e)
        out("initial_state {}".format(", ".join(["{}={}".format(a,i) for a,i in init])))

