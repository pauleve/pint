
SOURCES := \
	ph2thomas_coop.ml\
	ph2thomas_ig.ml\
	ph2thomas_param.ml\

PH2THOMAS_SOURCES := $(SOURCES:%=contrib/ph2thomas/%)
export PH2THOMAS_SOURCES


