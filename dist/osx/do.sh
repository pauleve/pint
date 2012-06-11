#!/bin/bash
if [ -z $1 ]; then
	echo "specify relname"
	exit 1
fi

LDFLAGS=-L$PWD/dist/osx CFLAGS=-I$PWD/dist/osx make dist-osx RELNAME=$1
