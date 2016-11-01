
OPAM_VERSION = $(shell opam --version 2>/dev/null)
ifneq ($(OPAM_VERSION),)
	HAS_OPAM = 1
endif

ifdef HAS_OPAM
	OCAML_LIBDIR := $(shell opam config var lib)/
else
	OCAML_LIBDIR := +
endif

all: ncl bcl

SOURCES := bes.mli bes.ml

LIBS = extLib

OCAML_DEFAULT_DIRS += $(OCAML_LIBDIR)extlib

RESULT = bes

install: rawinstall

include OCamlMakefile
# vi:set syntax=make:
