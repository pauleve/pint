
SOURCES := \
	../commonlib/util.ml\
	\
	ph_types.mli\
	ph_parser.mly\
	ph_lexer.mll\
	ph.ml\

LIBS := str

PHLIB_SOURCES := $(SOURCES:%=phlib/%) \
export PHLIB_SOURCES
PHLIB_LIBS := $(LIBS)
export PHLIB_LIBS

# vi:set syntax=make:
