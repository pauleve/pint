
SOURCES := \
	../commonlib/util.ml\
	../commonlib/param.ml\
	types.ml\
	graph.ml\
	polynome.ml\
	decision.ml\
	dynamic.ml\
	spec.ml\
	constraint.ml\
	valuation.ml\
	spig.ml\
	inference.ml\
	\
	brg.ml\
	logical.ml\
	goal.ml\
	spim.ml\

LIBS := nums str

PINTLIB_SOURCES := $(SOURCES:%=pintlib/%) \
export PINTLIB_SOURCES
PINTLIB_LIBS := $(LIBS)
export PINTLIB_LIBS

# vi:set syntax=make:
