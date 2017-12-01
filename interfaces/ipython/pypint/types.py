
import json
import re

re_int = re.compile(r"\d+")

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

def pint_value(value):
    """
    Returns string representation of a value for Pint
    """
    if isinstance(value, int):
        return str(value)
    else:
        return "\"{}\"".format(value)

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
        return " and ".join(["{}={}".format(*map(pint_value, it)) for it in self.items()])

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
        return "{} {} -> {}{}".format(*map(pint_value,
                (self.a, self.i, self.j, r_conds)))

    @property
    def modified_automata(self):
        """
        Set of automata in which the local transition takes place
        (``[a]``)
        """
        return set([self.a])

    @property
    def origins(self):
        return frozenset([(self.a, self.i)])

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
        assert len(aijs) > 1
        self.local_transitions = [tuple(aij) for aij in aijs]
        self.__automata = frozenset([a for (a,_,_) in aijs])
        self.conds = Conditions(conds)

    def __repr__(self):
        """
        Pint text representation of synchronized local transitions.
        """
        r_conds = " when %s" % self.conds if self.conds else ""
        return "{ %s }%s" % \
            (" ; ".join(["{} {} -> {}".format(*map(pint_value, aij)) \
                for aij in self.local_transitions]), r_conds)

    @property
    def modified_automata(self):
        """
        Set of automata in which the local transitions take place.
        """
        return set(self.__automata)

    @property
    def origins(self):
        return frozenset([(a,i) for (a,i,_) in self.local_transitions])


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

    Finally, alternative goals can be constructed using the operator ``|`` on
    the instanciated individual goals (see :py:meth:`.Goal.__or__`).


    Examples:

    >>> Goal(a=1)          # simple goal
    >>> Goal("a=1")        # equivalent to above
    >>> Goal(a=1,b=1)      # single sub-state goal
    >>> Goal("a=1,b=1")    # equivalent to above
    >>> Goal("a=1", "b=1") # sequence of simple goals
    >>> Goal({"a": 1, "b": 1}, {"a": 0})    # sequence of sub-state goals
    >>> Goal(a=1) | Goal(b=1)  # alternative goals
    """
    def __init__(self, *args, **kwargs):
        if args and kwargs:
            raise TypeError("Goal cannot be instanciated with both arguments and keywords")
        if not args and not kwargs:
            raise TypeError("Goal is empty")

        def parse_ls(s):
            a,i = s.split("=")
            a = a.strip().strip('"')
            i = i.strip()
            if re_int.fullmatch(i):
                i = int(i)
            else:
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
            if isinstance(args[0], Goal):
                self.__goals = []
                for arg in args:
                    if not isinstance(arg, Goal):
                        raise TypeError("Cannot mix Goal with other types in arguments")
                    self.__goals += arg.__goals
            else:
                self.__goals = [[goal_of_arg(arg) for arg in args]]
        if kwargs:
            self.__goals = [[kwargs]]

        self.__automata = set()
        for gs in self.__goals:
            for g in gs:
                self.__automata.update(g.keys())

    @classmethod
    def from_arg(self, arg, **kwargs):
        """
        Returns a `Goal` instance corresponding to `arg`
        """
        if isinstance(arg, self):
            return arg
        elif isinstance(arg, str) or isinstance(arg, dict):
            return self(arg)
        elif isinstance(arg, list):
            return self(*arg)
        elif isinstance(arg, tuple) and len(arg) == 2:
            return self({arg[0]: arg[1]})
        elif arg is None:
            return self(**kwargs)
        else:
            raise TypeError("Cannot convert a %s to %s" % (type(arg), self))

    def __or__(g1, g2):
        """
        Construction of *alternative* goals: the returned goal consists of the
        disjunction of `g1` and `g2` (at least one of the two goals have to be
        reached).

        :rtype: Goal
        """
        return Goal(g1, g2)

    @property
    def automata(self):
        """
        Returns the set of automata that are part of the goal specification
        """
        return self.__automata.copy()

    @property
    def is_simple_goal(self):
        """
        ``True`` iff the goal is only the local state of one automaton
        """
        return len(self.__goals) == 1 \
            and len(self.__goals[0]) == 1 \
            and len(self.__goals[0][0]) == 1 \

    def to_pint(self):
        """
        Returns the argument to append to Pint commands for goal specification
        """
        args = []
        for gs in self.__goals:
            if args:
                args.append("or")
            args += [",".join([pint_of_localstates([ai]) for ai in g.items()]) \
                        for g in gs]
        return " ".join(args)

    def __str__(self):
        return self.to_pint()


def pint_of_localstates(items):
    """
    Returns Pint text representation of a list of local states
    """
    def fmt_values(i):
        if type(i) is int:
            return [str(i)]
        elif type(i) is str:
            return ["\"%s\"" % i]
        else:
            return [fmt_values(j)[0] for j in i]
    def pint_of_keyvalue(a,i):
        return ["\"%s\"=%s" % (a,i) for i in fmt_values(i)]
    lss = []
    for a, i in items:
        lss += pint_of_keyvalue(a,i)
    return ",".join(lss)

__all__ = [
    "Inconc", "ternary",
    "Conditions", "LocalTransition", "SynchronizedLocalTransitions",
    "local_transition_from_json",
    "Goal",
    "pint_of_localstates",
]

