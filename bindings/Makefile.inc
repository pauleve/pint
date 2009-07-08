
SOURCES := \
	r_impl.c\
	r.idl\

CLIBS := m Rmath

BINDINGS_SOURCES := $(SOURCES:%=bindings/%)
export BINDINGS_SOURCES

# vi:set syntax=make:
