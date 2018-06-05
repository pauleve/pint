try:
    import boolean
except ImportError:
    print("ERROR: Please install boolean.py package (pip3 install boolean.py)")
    raise


class BooleanNetwork(dict):
    def __init__(self, *args, **kwargs):
        dict.__init__(self, *args, **kwargs)
        self.ba = boolean.BooleanAlgebra()
    def vars(self, names):
        return self.ba.symbols(*names)

    def to_pint(self):
        from colomoto_jupyter.sessionfiles import new_output_file
        from pypint.converters.lib.boolean_utils import BoolToAN
        from pypint.converters.lib.export_utils import pint_protect
        ba = self.ba

        def ls_of_lit(lit):
            if isinstance(lit, ba.NOT):
                return (lit.args[0].obj, 0)
            else:
                return (lit.obj, 1)

        anfile = new_output_file(ext="an")
        with open(anfile, "w") as f:

            def out(data):
                print(data, file=f)
            b2a = BoolToAN(ba, ls_of_lit, out)

            for a in sorted(self.keys()):
                out("{} [0, 1]".format(pint_protect(a)))

            for a, fa in sorted(self.items()):
                [sa] = self.vars([a])
                expr_up = fa.subs({sa: ba.FALSE}).simplify()
                expr_down = (~fa).subs({sa: ba.TRUE}).simplify()
                b2a.make_transitions([(a, 0, 1)], expr_up)
                b2a.make_transitions([(a, 1, 0)], expr_down)

        import pypint
        return pypint.load(anfile)

if __name__ == "__main__":
    bn = BooleanNetwork()
    a, b, c = bn.vars(["a", "b", "c"])
    bn["a"] = a
    bn["b"] = ~b
    bn["c"] = a | ~b
    an = bn.to_pint()
    print(an.source())


