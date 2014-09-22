
SOURCES := \
	automataNetwork.ml\
	an_export.ml\
	an_parser.mly\
	an_lexer.mll\
	an_input.mll\


#LIBS := 

ANLIB_SOURCES := $(SOURCES:%=anlib/%)
export ANLIB_SOURCES

#ANLIB_LIBS := $(LIBS)
#export ANLIB_LIBS

# vi:set syntax=make:
