
import re

pat_pred = re.compile("(\w+)\((.+)\)\.")
pat_reaction = re.compile("(?:(\w+):)?(?:(.*)\s+for\s+)?(.*[^<])\s*(<?=>)\s*(.*)\.")
pat_sto = re.compile("^(?:\d\*)?\(?([^\d\(\)][^\(\)]*)\)?$")

class model:
    def __init__(self):
        self.present = set()
        self.molecules = set()
        self.reactions = {}

    def new_anon_reaction():
        n = len(self.reactions)
        return "_anon%d" % n

    def register_reaction(self, name, reactants, products):
        assert (name not in self.reactions)
        self.molecules.update(reactants)
        self.molecules.update(products)
        self.reactions[name] = (reactants, products)

    @classmethod
    def from_file(celf, filename):

        def parse_hs(hs):
            def fmt_sto(a):
                a = a.strip()
                p = pat_sto.match(a)
                assert (p), (a,p)
                a = p.group(1)
                return a
            return [fmt_sto(a) for a in hs.split("+") if a != "_"]

        m = celf()


        with open(filename) as f:
            for l in f:
                l = l.strip()
                if not l or l.startswith("%"):
                    continue
                mp = pat_pred.match(l)
                if mp:
                    pred = mp.group(1)
                    args = [a.strip() for a in mp.group(2).split(",")]
                    if pred == "present":
                        m.present.add(args[0])
                    elif pred == "absent":
                        try:
                            m.present.remove(args[0])
                        except KeyError:
                            pass
                else:
                    mr = pat_reaction.match(l)
                    if mr:
                        name, kinetics, reactants, direction, products = mr.groups()
                        if name is None:
                            name = m.new_anon_reaction()
                        reactants = parse_hs(reactants)
                        products = parse_hs(products)
                        if direction == "<=>":
                            if kinetics:
                                assert (kinetics[0]=="(" and kinetics[-1]==")")
                                kinetics = [k.strip() for k in kinetics[1:-1].split(",")]
                            if not kinetics or kinetics[0] != "0":
                                m.register_reaction(name, reactants, products)
                            if not kinetics or kinetics[1] != "0":
                                m.register_reaction("%s_rev" % name, products, reactants)
                        else:
                            if kinetics:
                                kinetics = kinetics.strip()
                            if not kinetics or kinetics != "0":
                                m.register_reaction(name, reactants, products)

        return m

    def an_name(self, molecule):
        return '"%s"' % molecule.replace("::","__")

    def to_an(self, fd):
        n = self.an_name

        def fmt_cond(conds):
            s = " and ".join(["%s=1" % n(c) for c in conds])
            if s:
                return " when %s" % s
            return ""

        for a in self.molecules:
            print("%s [0,1]" % n(a), file=fd)
        for e, (conds, res) in sorted(self.reactions.items()):
            print("%s [0,1,2]" % n(e), file=fd)
            print("%s 0 -> 1%s" % (n(e), fmt_cond(conds)), file=fd)
            print("%s 1 -> 2%s" % (n(e), fmt_cond(res)), file=fd)
            print("%s 2 -> 0" % n(e), file=fd)
            for a in conds:
                print("%s 1 -> 0 when %s=2" % (n(a), n(e)), file=fd)
            for a in res:
                print("%s 0 -> 1 when %s=1" % (n(a), n(e)), file=fd)

        init = ", ".join(["%s=1" % n(a) for a in self.present])
        print("initial_state %s" % init, file=fd)

def import_biocham(localfile, outfd):
    m = model.from_file(localfile)
    m.to_an(outfd)

