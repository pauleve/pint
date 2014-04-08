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

install: phc-install phstat-install phstable-install phreach-install phexec-install ph2thomas-install misc-install

pre-release:
	sed -i 's/:.*##VERSION##/: "$(RELNAME)",##VERSION##/' setup.py
	DEBEMAIL="loic.pauleve@ens-cachan.org" DEBFULLNAME="Loic Pauleve" debchange -v $(RELNAME) Release $(RELNAME)
	DEBEMAIL="loic.pauleve@ens-cachan.org" DEBFULLNAME="Loic Pauleve" debchange -r --distribution unstable
	git commit -a -m "release $(RELNAME)"

release:
	git tag $(RELNAME)
	git archive -o ../pint-$(RELNAME).zip --prefix pint-$(RELNAME)/ $(RELBRANCH)

OSX_W=/tmp/osx-pint/pint-$(RELNAME)
OSX_W_BIN=/tmp/osx-pint/pint-$(RELNAME)/pint
OSX_W_SHARE=$(OSX_W_BIN)/share
OSX_BINS=phc ph-stat ph-stable ph-reach ph-exec ph2thomas

# should be called using ./dist/osx/do.sh
dist-osx: phc phstat phstable phreach phexec ph2thomas
	-rm -rf $(OSX_W)
	install -d $(OSX_W_BIN)
	install -m 755 $(OSX_BINS:%=bin/%) $(MISC_TOOLS) $(OSX_W_BIN)
	install -m 644 dist/osx/*.dylib $(OSX_W_BIN)
	for i in $(OSX_BINS); do \
		install -m 755 -b -B .mac dist/osx/wrapper.sh $(OSX_W_BIN)/$$i; \
	done
	install -d $(OSX_W_SHARE)/contrib/ph2thomas
	install -m 644 contrib/ph2thomas/*.lp $(OSX_W_SHARE)/contrib/ph2thomas
	OSX_BINS="$(OSX_BINS)" MISC_TOOLS="$(MISC_TOOLS)" ./dist/osx/gen_install.sh > $(OSX_W_BIN)/install.sh
	chmod 655 $(OSX_W_BIN)/install.sh
	install -d $(OSX_W)/examples
	install -m 644 examples/* $(OSX_W)/examples
	install -m 644 dist/osx/README $(OSX_W)
	-rm -f /tmp/pint-$(RELNAME).dmg
	hdiutil create -srcfolder $(OSX_W) -volname pint-$(RELNAME) -fs HFS+ /tmp/pint-$(RELNAME).dmg

dist-deb:
	dpkg-buildpackage -d	

