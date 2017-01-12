
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
        return "\"%s\" %d -> %d when %s" % \
                (self.a, self.i, self.j, self.conds)




