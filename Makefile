all: pint phstable phreach phc libpint phstat phexec ph2thomas

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
ph2thomas:
	make -f target/ph2thomas

clean:
	make -f target/phexec clean
	make -f target/ph2thomas clean
	make -f target/phreach clean
	make -f target/phstable clean
	make -f target/phstat clean
	make -f target/phc clean
	make -f target/pinttop clean
	make -f target/pintlib clean

apidoc:
	rm -f docs/api/*
	ocamldoc -sort -html -d docs/api -I bindings -I pintlib -I phlib \
		-t "Pint OCaml API - process.hitting.free.fr" \
		bindings/r.mli pintlib/*.mli phlib/*.mli *.mli


RELNAME=$(shell date -I)
RELBRANCH=master

PREFIX=/usr
DESTDIR=

phc-install: phc
	make -f target/phc PREFIX=$(PREFIX) DESTDIR=$(DESTDIR) install
#pint-install: pint
#	make -f target/pinttop PREFIX=$(PREFIX) DESTDIR=$(DESTDIR) install
libpint-install: libpint
	make -f target/pintlib PREFIX=$(PREFIX) DESTDIR=$(DESTDIR) install
phstat-install: phstat
	make -f target/phstat PREFIX=$(PREFIX) DESTDIR=$(DESTDIR) install
phstable-install: phstable
	make -f target/phstable PREFIX=$(PREFIX) DESTDIR=$(DESTDIR) install
phreach-install: phreach
	make -f target/phreach PREFIX=$(PREFIX) DESTDIR=$(DESTDIR) install
phexec-install: phexec
	make -f target/phexec PREFIX=$(PREFIX) DESTDIR=$(DESTDIR) install
ph2thomas-install: ph2thomas
	make -f target/ph2thomas PREFIX=$(PREFIX) DESTDIR=$(DESTDIR) install

MISC_TOOLS = converters/bcx2ph converters/CNA2ph converters/ginml2ph
misc-install:
	for i in $(MISC_TOOLS); do \
		install -m 0755 $$i $(DESTDIR)$(PREFIX)/bin; \
	done

install: phc-install libpint-install phstat-install phstable-install phreach-install phexec-install ph2thomas-install misc-install

release:
	git tag $(RELNAME)
	git archive -o /tmp/pint-$(RELNAME).zip --prefix pint-$(RELNAME)/ $(RELBRANCH)

