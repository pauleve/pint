
def pint_protect(data):
    if isinstance(data, str):
        return '"%s"' % data
    else:
        return str(data)

def pint_of_localtransition(changes, conds):
    changes = ["{} {} -> {}".format(*map(pint_protect, c)) for c in changes]
    if len(changes) > 1:
        changes = "{ %s }" % " ; ".join(changes)
    else:
        changes = changes[0]
    conds = dict(conds)
    conds = ["{}={}".format(*map(pint_protect, it)) for it in conds.items()]
    conds = " when %s" % " and ".join(conds) if conds else ""
    return changes + conds

