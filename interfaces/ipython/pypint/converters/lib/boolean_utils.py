
from functools import partial

from .export_utils import pint_of_localtransition

try:
    import boolean
except ImportError:
    print("ERROR: Please install boolean.py package (pip3 install boolean.py)")
    raise

class BoolToAN:
    def __init__(self, ba, ls_of_lit, out, subs=None):
        self.ba = ba
        self.ls_of_lit = ls_of_lit
        self.out = out
        self.subs = subs

    @staticmethod
    def BooleanAlgebra():
        return boolean.BooleanAlgebra()

    def make_transitions(self, changes, expr):
        if expr == self.ba.FALSE:
            return
        LT = partial(pint_of_localtransition, changes)

        dnf = self.ba.dnf(expr)
        if self.subs:
            # substitution can break the DNF form
            expr = dnf.subs(self.subs)
            dnf = self.ba.dnf(expr)

        if isinstance(dnf, self.ba.OR):
            clauses = dnf.args
        elif isinstance(dnf, self.ba.AND):
            clauses = [dnf]
        else:
            clauses = [dnf]
        for clause in clauses:
            if isinstance(clause, self.ba.AND):
                lits = clause.args
            elif clause == self.ba.TRUE:
                lits = None
            else:
                lits = [clause]
            conds = [self.ls_of_lit(l) for l in lits] if lits else []
            self.out(LT(conds))

