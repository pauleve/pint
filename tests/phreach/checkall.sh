#!/usr/bin/env bash
for x in *.cmd; do
	n="`basename $x .cmd`"
	echo -n "$n..."

	sout=`mktemp`
	serr=`mktemp`
	$(<$x) >$sout 2>$serr

	if diff -q $n.stdout $sout > /dev/null; then
		echo -n " [stdout ok]"
		rm -f $sout
	else
		echo -n " [STDOUT KO]"
		mv -b $sout $n.stdout.err
	fi
	if diff -q $n.stderr $serr > /dev/null; then
		echo -n " [stderr ok]"
		rm -f $serr
	else
		echo -n " [STDERR KO]"
		mv -b $serr $n.stderr.err
	fi
	echo
done
