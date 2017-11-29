
try:
    import boolean
except ImportError:
    print("ERROR: Please install boolean.py package (pip3 install boolean.py)")
    raise

class BoolToAN:
    def __init__(self, ba, cond_of_lit, out, subs=None):
        self.ba = ba
        self.cond_of_lit = cond_of_lit
        self.out = out
        self.subs = subs

    @staticmethod
    def BooleanAlgebra():
        return boolean.BooleanAlgebra()

    def make_transitions(self, changes, expr):
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

        changes = ["{} {} -> {}".format(*c) for c in changes]
        if len(changes) == 1:
            changes = changes[0]
        else:
            changes = "{ %s }" % " ; ".join(changes)

        for clause in clauses:
            cond = ""
            if isinstance(clause, self.ba.AND):
                lits = clause.args
            elif clause == self.ba.TRUE:
                lits = None
            else:
                lits = [clause]
            if lits:
                def strlit(lit):
                    if isinstance(lit, self.ba.NOT):
                        return self.cond_of_lit(lit.args[0].obj, False)
                    else:
                        return self.cond_of_lit(lit.obj, True)
                cond = " when %s" % " and ".join([strlit(l) for l in lits])
            self.out("%s%s" % (changes, cond))

