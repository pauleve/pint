
all: ncl bcl

SOURCES := bes.mli bes.ml

LIBS = extLib

INCDIRS += $(shell $(OCAMLFIND) query extlib)

RESULT = bes

install: rawinstall

include OCamlMakefile
# vi:set syntax=make:
