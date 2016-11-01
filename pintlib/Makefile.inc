
SOURCES := \
	pintTypes.mli\
	pintTypes.ml\
	debug.mli\
	debug.ml\
	param.mli\
	param.ml\
	kSets.ml\
	util.mli\
	util.ml\
	interactionGraph.ml\
	valSet.ml\
	aSP_solver.ml\
	pn_mci.ml\


LIBS := bigarray unix nums str batteries bes
ifeq "$(BYTECODE)" "1"
else
LIBS := extLib $(LIBS)
endif

OCAML_DEFAULT_DIRS += $(OCAML_LIBDIR)batteries \
	$(OCAML_LIBDIR)extlib \
	3rdparty/bes/src

PINTLIB_SOURCES := $(SOURCES:%=pintlib/%)
export PINTLIB_SOURCES

PINTLIB_LIBS := $(LIBS)
export PINTLIB_LIBS

# vi:set syntax=make:
