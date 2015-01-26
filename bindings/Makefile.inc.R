
SOURCES := \
	r_impl.c\
	r.idl\

CLIBS := m Rmath

BINDINGS_SOURCES := $(SOURCES:%=bindings/%)
export BINDINGS_SOURCES

ifdef HAS_OPAM
OCAML_DEFAULT_DIRS += $(OCAML_LIBDIR)camlidl
export OCAML_DEFAULT_DIRS
endif

# vi:set syntax=make:
