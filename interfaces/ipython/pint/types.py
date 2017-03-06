
import json

class Inconc:
    """
    Inconclusive type (in opposition to `True` and `False`).
    It rises a `TypeError` when used in a Boolean expression.
    """
    def __init__(self):
        raise TypeError("Inconc should not be instantiated")
    def __bool__(self):
        raise TypeError("Inconc is not a Boolean")
    __nonzero__ = __bool__

def ternary(b):
    """
    Returns ternary variable representation of `b`: either `True`, `False`, or
    :py:class:`.Inconc`. If `b` is `None`, returns `Inconc`.
    """
    if b is None:
        return Inconc
    elif b in [True,False,Inconc]:
        return b
    else:
        raise TypeError("'%s' is not a valid ternary" % repr(b))

class Conditions(dict):
    """
    Used to represent conditions of transitions.
    They are modeled as standard dictionnary, associating automata to the
    required local states.
    """
    def __init__(self, *args):
        super(Conditions, self).__init__(*args)
    def __str__(self):
        """
        Pint text representation of a condition.
        """
        return " and ".join(["\"%s\"=%s" % it for it in self.items()])

class LocalTransition(object):
    """
    A local transition within a single automaton.

    An instance has 4 attributes:

    * `a`: name of the automaton
    * `i`: initial local state of `a`
    * `j`: final local state of `a` after the transition
    * `conds`: :py:class:`.Conditions` object
    """
    def __init__(self, a, i, j, conds):
        """
        Transition `a` `i` -> `j` when `conds`, where `conds` is a `dict`-like
        object.
        """
        self.a = a
        self.i = i
        self.j = j
        self.conds = Conditions(conds)

    def __repr__(self):
        """
        Pint text representation of a local transition
        """
        r_conds = " when %s" % self.conds if self.conds else ""
        return "\"%s\" %d -> %d%s" % \
                (self.a, self.i, self.j, r_conds)

    @property
    def modified_automata(self):
        """
        Set of automata in which the local transition takes place
        (``[a]``)
        """
        return set([self.a])

class SynchronizedLocalTransitions(object):
    """
    Synchronized local transitions.

    An instance has 2 attributes:

    * `local_transitions`: a list of `(a,i,j)` tuples representing the local
      transitions acting (always) synchronously
    * `conds`: :py:class:`.Conditions` object
    """
    def __init__(self, aijs, conds):
        """
        `aijs` is a list of `(a,i,j)` tuples and `conds` a `dict`-like object.
        """
        self.local_transitions = [tuple(aij) for aij in aijs]
        self.__automata = frozenset([a for (a,_,_) in aijs])
        self.conds = Conditions(conds)

    def __repr__(self):
        """
        Pint text representation of synchronized local transitions.
        """
        r_conds = " when %s" % self.conds if self.conds else ""
        return "{ %s }%s" % \
            (" ; ".join(["\"%s\" %d -> %d" % aij \
                for aij in self.local_transitions]), r_conds)

    @property
    def modified_automata(self):
        """
        Set of automata in which the local transitions take place.
        """
        return set(self.__automata)


def local_transition_from_json(tup):
    """
    Converts a JSON representation of a local transition (as return by Pint
    executables) to either :py:class:`.LocalTransition` or
    :py:class:`.SynchronizedLocalTransitions`.
    """
    if len(tup) == 4:
        return LocalTransition(*tup)
    elif len(tup) == 2:
        return SynchronizedLocalTransitions(*tup)
    raise ValueError("%s: Invalid tuple for local transition" % tup)


def goal_automata(goal):
    """
    Returns the set of automata referenced in `goal`
    """
    # TODO type goal specification
    a,_ = goal.split("=")
    a = a.strip('"')
    return set([a])

__all__ = [
    "Inconc", "ternary",
    "Conditions", "LocalTransition", "SynchronizedLocalTransitions",
    "local_transition_from_json",
    "goal_automata",
]

