
SOURCES := \
	../commonlib/util.ml\
	\
	ph_types.ml\
	ph_parser.mly\
	ph_lexer.mll\
	ph_translator.ml\
	ph_verif.ml\
	ph_util.ml\

LIBS := str

PHLIB_SOURCES := $(SOURCES:%=phlib/%) \
export PHLIB_SOURCES
PHLIB_LIBS := $(LIBS)
export PHLIB_LIBS

# vi:set syntax=make:
