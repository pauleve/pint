
SOURCES := \
	automataNetwork.ml\
	an_fixpoint.ml\
	an_localpaths.ml\
	localCausalityGraph.ml\
	an_transformers.ml\
	an_reach_asp.ml\
	an_reach.ml\
	an_stategraph.ml\
	an_export.ml\
	an_bifurcations.ml\
	an_reprogramming.ml\
	an_parser.mly\
	an_lexer.mll\
	an_input.ml\
	an_cli.ml\
	#an_focal.ml\
	an_snippets.ml\

LIBS := facile zarith

ANLIB_SOURCES := $(SOURCES:%=anlib/%)
export ANLIB_SOURCES

ANLIB_LIBS := $(LIBS)
export ANLIB_LIBS

# vi:set syntax=make:
