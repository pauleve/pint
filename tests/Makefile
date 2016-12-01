all: nc

TESTS_SOURCES := toto \
	TestCommon.ml \
	TestPintReach.ml \
	tests.ml

SOURCES := $(TESTS_SOURCES:%=tests/%)
RESULT = tests/pinttests

CLIBS := m Rmath camlidl
LIBS := pint
LDFLAGS := -L.

PACKS := oUnit

OCAMLFLAGS = -I pintlib -I phlib

include OCamlMakefile


