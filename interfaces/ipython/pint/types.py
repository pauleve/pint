
import json

class Inconc:
    def __bool__(self):
        raise TypeError("Inconc is not a Boolean")
    __nonzero__ = __bool__

def ternary(b):
    if b is None:
        return Inconc
    elif b is True or b is False:
        return b
    else:
        raise TypeError("'%s' is not a valid ternary" % repr(b))

class Conditions(dict):
    def __init__(self, *args):
        super(Conditions, self).__init__(*args)
    def __str__(self):
        return " and ".join(["\"%s\"=%s" % it for it in self.items()])

class LocalTransition(object):
    def __init__(self, a, i, j, conds):
        self.a = a
        self.i = i
        self.j = j
        self.conds = Conditions(conds)

    def __repr__(self):
        r_conds = " when %s" % self.conds if self.conds else ""
        return "\"%s\" %d -> %d%s" % \
                (self.a, self.i, self.j, r_conds)

    @property
    def modified_automata(self):
        return set([self.a])

class SynchronizedLocalTransitions(object):
    def __init__(self, aijs, conds):
        self.local_transitions = [tuple(aij) for aij in aijs]
        self.__automata = set([a for (a,_,_) in aijs])
        self.conds = Conditions(conds)

    def __repr__(self):
        r_conds = " when %s" % self.conds if self.conds else ""
        return "{ %s }%s" % \
            (" ; ".join(["\"%s\" %d -> %d" % aij \
                for aij in self.local_transitions]), r_conds)

    @property
    def modified_automata(self):
        return self.__automata


def local_transition_from_json(tup):
    if len(tup) == 4:
        return LocalTransition(*tup)
    elif len(tup) == 2:
        return SynchronizedLocalTransitions(*tup)
    raise ValueError("%s: Invalid tuple for local transition" % tup)


def goal_automata(goal):
    """
    Returns the set of automata referenced in the given `goal`
    """
    # TODO type goal specification
    a,_ = goal.split("=")
    a = a.strip('"')
    return set([a])
