
SOURCES := \
	r_impl.c\
	r.idl\

BINDINGS_CLIBS := m Rmath

BINDINGS_SOURCES := $(SOURCES:%=bindings/%)
export BINDINGS_SOURCES

INCDIRS += $(shell $(OCAMLFIND) query camlidl)
export INCDIRS

# vi:set syntax=make:
