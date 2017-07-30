
SOURCES := \
	pintmeta.ml\
	pintTypes.mli\
	pintTypes.ml\
	debug.mli\
	debug.ml\
	kSets.ml\
	util.mli\
	util.ml\
	interactionGraph.ml\
	valSet.ml\
	pintenv.ml\
	aSP_parser.mly\
	aSP_lexer.mll\
	aSP_solver.ml\
	pn_mci.ml\
	#param.mli\
	param.ml\


LIBS := bigarray unix nums str bes
ifeq "$(BYTECODE)" "1"
else
LIBS := extLib $(LIBS)
endif

INCDIRS += $(shell $(OCAMLFIND) query extlib) 3rdparty/bes/src

PINTLIB_SOURCES := $(SOURCES:%=pintlib/%)
export PINTLIB_SOURCES

PINTLIB_LIBS := $(LIBS)
export PINTLIB_LIBS

# vi:set syntax=make:
