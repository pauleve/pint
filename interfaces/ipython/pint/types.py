
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


class Goal:
    """
    Object which represents reachability goal used in most of Pint commands.

    In its simplest form, a goal is the local state of an automaton.
    A goal can also be a sequence of sub-states.

    It can be instanciated either with arguments or keywords (but not both).

    If instanciated with arguments, each argument specifies a sub-state, and the
    goal is the sequence of these sub-states.
    A sub-state can be specified either by a string (in pint format), or by a
    `dict`-like object.

    If instanciated with keywords, the goal is a single sub-state where the keys
    correspond to the automata and the values their local states.


    Examples:

    >>> Goal(a=1)          # simple goal
    >>> Goal("a=1")        # equivalent to above
    >>> Goal(a=1,b=1)      # single sub-state goal
    >>> Goal("a=1,b=1")    # equivalent to above
    >>> Goal("a=1", "b=1") # sequence of simple goals
    >>> Goal({"a": 1, "b": 1}, {"a": 0})    # sequence of sub-state goals
    """
    def __init__(self, *args, **kwargs):
        if args and kwargs:
            raise TypeError("Goal cannot be instanciated with both arguments and keywords")
        if not args and not kwargs:
            raise TypeError("Goal is empty")

        def parse_ls(s):
            a,i = s.split("=")
            a = a.strip('"')
            i = i.strip('"')
            return a,i

        def goal_of_arg(a):
            if isinstance(a, str):
                return dict(map(parse_ls, a.split(",")))
            elif isinstance(a, dict):
                return a.copy()
            else:
                raise TypeError("Goal: do not support %s as argument" % type(a))

        if args:
            self.__goals = [goal_of_arg(arg) for arg in args]
        if kwargs:
            self.__goals = [kwargs]

        self.__automata = frozenset()
        for g in self.__goals:
            self.__automata.update(g.keys())

    @classmethod
    def from_arg(celf, arg, **kwargs):
        """
        Returns a `Goal` instance corresponding to `arg`
        """
        if isinstance(arg, celf):
            return celf
        elif isinstance(arg, str):
            return celf(str)
        elif isinstance(arg, list):
            return celf(*arg)
        elif arg is None:
            return celf(**kwargs)
        else:
            raise TypeError("Cannot convert a %s to %s" % (type(arg), celf))

    @property
    def automata(self):
        """
        Returns the set of automata that are part of the goal specification
        """
        return self.__automata

    @property
    def is_simple_goal(self):
        """
        ``True`` iff the goal is only the local state of one automaton
        """
        return len(self.__goals) == 1 and len(self.__goals[0]) == 1

    def pint_args(self):
        """
        Returns the list of arguments to append to Pint commands for goal
        specification
        """
        return [",".join(["%s=%s" % ai for ai in g.items()]) \
                    for g in self.__goals]


__all__ = [
    "Inconc", "ternary",
    "Conditions", "LocalTransition", "SynchronizedLocalTransitions",
    "local_transition_from_json",
    "Goal",
]

