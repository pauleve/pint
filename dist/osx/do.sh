#!/bin/bash
if [ -z $1 ]; then
	echo "specify relname"
	exit 1
fi

python setup.py --share-path /usr/local/share/pint
LDFLAGS=-L$PWD/dist/osx CFLAGS=-I$PWD/dist/osx make dist-osx RELNAME=$1
