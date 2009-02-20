all: pint ph2spim

ph2spim:
	make -f target/ph2spim
pint:
	make -f target/pinttop

clean:
	make -f target/ph2spim clean
	make -f target/pinttop clean

exportph: clean
	-rm -rf dist/ph2spim
	mkdir -p dist/ph2spim
	cp -rv commonlib phlib ph2spim.ml dist/ph2spim
	cp target/ph2spim dist/ph2spim/Makefile
	cp OCamlMakefile dist/ph2spim
	mkdir dist/ph2spim/bin

