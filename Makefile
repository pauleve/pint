all: pint ph2spim

ph2spim:
	make -f target/ph2spim
pint:
	make -f target/pinttop

clean:
	make -f target/ph2spim clean
	make -f target/pinttop clean



