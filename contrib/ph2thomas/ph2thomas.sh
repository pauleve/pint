#!/bin/bash

if [ -z "$1" ]
then
  echo "Syntax: sh ph2thomas.sh <PH file> [ASP file [data file [DOT file [AB|active|enum]]]]" >&2
  echo "The program computes as far as alowed by the given arguments" >&2
  exit 1
fi

echo "Calling ph2lp.ml ..." >&2
pint ph2lp.ml $1 $2

if [ -z "$2" ]
then
  exit 0
fi

echo "Calling (phinfercoop1.lp ; phinfercoop2.lp) | phinfercoop.ml ..." >&2
( clingo 0 --verbose=0 phinfercoop1.lp $2 ; \
  clingo 0 --verbose=0 phinfercoop2.lp $2 ) \
| pint phinfercoop.ml $2

if [ -z "$3" ]
then
  exit 0
fi

echo "Calling phinferIG.lp | phinferIG.ml ..." >&2
clingo 0 --verbose=0 phinferIG.lp $2 \
| pint phinferIG.ml $2 $3 $4

if [ -z "$5" ]
then
  exit 0
fi

echo "Calling phinferK.lp | phinferK.ml ..." >&2
clingo 0 --verbose=0 phinferK.lp $2 \
| pint phinferK.ml $2 $3 $5

if [ "$5" != "enum" ]
then
  exit 0
fi

echo "Calling phenumK.lp ..." >&2
clingo 0 --verbose=0 phenumK.lp $2

