#!/usr/bin/env bash
for x in *.cmd; do
	n="`basename $x .cmd`"
	echo -n "$n..."

	sout=`mktemp`
	serr=`mktemp`
	$(<$x) >$sout 2>$serr

	if diff -q $n.stdout $sout > /dev/null; then
		echo -n " [stdout ok]"
	else
		echo -n " [STDOUT KO]"
	fi
	if diff -q $n.stderr $serr > /dev/null; then
		echo -n " [stderr ok]"
	else
		echo -n " [STDERR KO]"
	fi
	echo

	rm -f $serr $sout
done
