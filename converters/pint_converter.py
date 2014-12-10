
from subprocess import *

def pint_export():
    p = Popen(["pint-export", "--simplify"], stdin=PIPE)
    out = p.stdin
    def output(data):
        out.write(data.encode("utf-8"))
    def done():
        out.close()
        p.wait()
    return output, done


class State:
    def __init__(self, sd, nodes):
        self.nodes = nodes
        self.ml = dict([(a,max(sd[a])) for a in nodes])
    def next(self, x):
        end = True
        for i in range(len(self.nodes)):
            if x[i] == self.ml[self.nodes[i]]:
                x[i] = 0
            else:
                end = False
                x[i] += 1
                break
        return not end

    def __iter__(self):
        x = [0]*len(self.nodes)
        yield tuple(x)
        while self.next(x):
            yield tuple(x)

def Kinit(sd, a):
    return dict([(i, set()) for i in sd[a]])

def K2an(sd, dep, K, init=None):
    output, done = pint_export()
    for a in sorted(sd.keys()):
        output("%s [%s]\n" % (a, ", ".join(map(str,sd[a]))))
    for a in sorted(sd.keys()):
        if a not in K:
            continue
        bs = dep[a]
        bs_a = bs.index(a) if a in bs else None
        for incr in [1, -1]:
            if incr > 0:
                r = sd[a][1:]
            else:
                r = sd[a][:-1]
            for i in r:
                myK = set()
                if incr > 0:
                    ri = sd[a][i:]
                else:
                    ri = sd[a][:i+1]
                for j in ri:
                    conds = K[a][j]
                    if bs_a is not None:
                        conds = filter(lambda x: x[bs_a] == i-incr, conds)
                    myK.update(conds)
                for x in myK:
                    v = zip(bs,x)
                    sv = " and ".join(["%s=%s" % (b,j) for (b,j) in v if b != a])
                    if sv:
                        sv = " when %s" % sv
                    output("%s %d -> %d%s\n" % (a,i-incr,i,sv))
    init = [(a,i) for (a,i) in init if i > 0]
    if init:
        output("initial_state %s\n" % (", ".join(["%s=%s" % iv for iv in init])))
    done()


