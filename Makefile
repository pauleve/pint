all: pint phstable phreach phc libpint phstat phexec

phc:
	make -f target/phc
pint:
	make -f target/pinttop
libpint:
	make -f target/pintlib
phstat:
	make -f target/phstat
phstable:
	make -f target/phstable
phreach:
	make -f target/phreach
phexec:
	make -f target/phexec

clean:
	make -f target/phexec clean
	make -f target/phreach clean
	make -f target/phstable clean
	make -f target/phstat clean
	make -f target/phc clean
	make -f target/pinttop clean
	make -f target/pintlib clean

apidoc:
	rm -f docs/api/*
	ocamldoc -html -d docs/api -I bindings -I pintlib -I phlib \
		-t "Pint OCaml API - processhitting.wordpress.com" \
		bindings/r.mli pintlib/*.mli phlib/*.mli

