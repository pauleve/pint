
SOURCES := \
	bool.ml\
	debug.ml\
	param.ml\
	util.ml\

LIBS := 

PINTLIB_SOURCES := $(SOURCES:%=pintlib/%)
export PINTLIB_SOURCES

PINTLIB_LIBS := $(LIBS)
export PINTLIB_LIBS

# vi:set syntax=make:
