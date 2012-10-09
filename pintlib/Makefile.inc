
SOURCES := \
	debug.mli\
	debug.ml\
	param.mli\
	param.ml\
	kSets.ml\
	util.mli\
	util.ml\

LIBS := 

PINTLIB_SOURCES := $(SOURCES:%=pintlib/%)
export PINTLIB_SOURCES

PINTLIB_LIBS := $(LIBS)
export PINTLIB_LIBS

# vi:set syntax=make:
