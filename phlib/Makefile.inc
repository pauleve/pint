
SOURCES := \
	ph_types.ml\
	ph_sortgraph.ml\
	ph_parser.mly\
	ph_lexer.mll\
	ph_translator.ml\
	ph_util.ml\
	ph_op.ml\
	ph_static.ml\
	ph_reach.ml\

LIBS := str nums

PHLIB_SOURCES := $(SOURCES:%=phlib/%)
export PHLIB_SOURCES
PHLIB_LIBS := $(LIBS)
export PHLIB_LIBS

# vi:set syntax=make:
