
SOURCES := \
	ph_types.mli\
	ph_types.ml\
	ph_useropts.ml\
	ph_instance.ml\
	ph_parser.mly\
	ph_lexer.mll\
	ph_translator.mli\
	ph_translator.ml\
	ph_util.mli\
	ph_util.ml\
	ph_fixpoint.ml\
	ph_static.mli\
	ph_static.ml\
	ph_bounce_seq.ml\
	ph_abstr_struct.ml\
	ph_reach.mli\
	ph_reach.ml\
	ph_machine.mli\
	ph_machine.ml\

LIBS := str nums facile

PHLIB_SOURCES := $(SOURCES:%=phlib/%)
export PHLIB_SOURCES
PHLIB_LIBS := $(LIBS)
export PHLIB_LIBS

OCAML_DEFAULT_DIRS := +facile
export OCAML_DEFAULT_DIRS

# vi:set syntax=make:
