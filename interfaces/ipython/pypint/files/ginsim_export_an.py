assert "lqm" in globals(), "GINsim >= 2.9.6 is required. Please upgrade"
ifmt = gs.args[0]
ifile = gs.args[1]
ofile = gs.args[2]
if ifmt == "ginsim":
    model = gs.open(ifile).getModel()
else:
    model = lqm.loadModel(ifile, ifmt)
lqm.saveModel(model, ofile, "an")
