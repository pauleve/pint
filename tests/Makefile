all: nc

TESTS_SOURCES := toto \
	TestCommon.ml \
	TestPhReach.ml \
	tests.ml

SOURCES := $(TESTS_SOURCES:%=tests/%)
RESULT = tests/pinttests

CLIBS := m Rmath camlidl
LIBS := pint
LDFLAGS := -L.

PACKS := oUnit

OCAML_DEFAULT_DIRS := +oUnit
OCAMLFLAGS = -I phlib

include OCamlMakefile


