all: pint phstable phreach phc libpint phstat

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

clean:
	make -f target/phreach clean
	make -f target/phstable clean
	make -f target/phstat clean
	make -f target/phc clean
	make -f target/pinttop clean
	make -f target/pintlib clean

exportphc: clean
	-rm -rf dist/phc
	mkdir -p dist/phc
	cp -rv commonlib phlib phc.ml dist/phc
	cp target/phc dist/phc/Makefile
	cp OCamlMakefile dist/phc
	cp README-PHC.txt disc/phc/README.txt
	mkdir dist/phc/bin

