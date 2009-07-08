
SOURCES := \
	util.ml\
	param.ml\

LIBS := 

COMMON_SOURCES := $(SOURCES:%=commonlib/%)
export COMMON_SOURCES

COMMON_LIBS := $(LIBS)
export COMMON_LIBS

# vi:set syntax=make:
