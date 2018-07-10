from colomoto_jupyter.sessionfiles import new_output_file
from pypint.converters.lib.boolean_utils import BoolToAN
from pypint.converters.lib.export_utils import pint_protect

from colomoto.minibn import BooleanNetwork, MultiValuedNetwork

def import_minibn(f):
    if isinstance(f, MultiValuedNetwork):
        raise Exception("Direct conversion from MultiValuedNetwork is not supported yet (you should use biolqm instead)")

    assert isinstance(f, BooleanNetwork), \
        "{}: only objects of type {} are supported".format(import_minibn, BooleanNetwork)

    ba = f.ba

    def ls_of_lit(lit):
        if isinstance(lit, ba.NOT):
            return (lit.args[0].obj, 0)
        else:
            return (lit.obj, 1)

    anfile = new_output_file(ext="an")
    with open(anfile, "w") as outf:
        def out(data):
            print(data, file=outf)
        b2a = BoolToAN(ba, ls_of_lit, out)

        for a in sorted(f.keys()):
            out("{} [0, 1]".format(pint_protect(a)))

        for a, fa in sorted(f.items()):
            sa = f.vars(a)[0]
            expr_up = fa.subs({sa: ba.FALSE}).literalize().simplify()
            expr_down = (~fa).subs({sa: ba.TRUE}).literalize().simplify()
            b2a.make_transitions([(a, 0, 1)], expr_up)
            b2a.make_transitions([(a, 1, 0)], expr_down)

    import pypint
    return pypint.load(anfile)

