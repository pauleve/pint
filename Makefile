
TARGETS=\
	libpint\
	pinttop\
	pint-config\
	pint-export\
	pint-its\
	pint-lcg\
	pint-mole\
	pint-nusmv\
	pint-reach\
	pint-sg\
	pint-stable\
	phc\
	ph-exec\
	ph2thomas\
	3rdparty_runtime\

OSX_TARGETS=\
	pint-config\
	pint-export\
	pint-its\
	pint-lcg\
	pint-mole\
	pint-nusmv\
	pint-reach\
	pint-sg\
	pint-stable\
	phc\
	ph-exec\
	ph2thomas\
	3rdparty_runtime\


MISC_TOOLS = \
	bin/pint_install_deps\
	converters/pint_converter.py\
	converters/bcx2ph\
	converters/CNA2an\


.PHONY: $(TARGETS) aspfiles 3rdparty

all: depcheck 3rdparty $(TARGETS)

ifndef OCAMLFIND
  OCAMLFIND := ocamlfind
endif
export OCAMLFIND

depcheck:
	@$(OCAMLFIND) query extlib>/dev/null

3rdparty_clean:
	make -C 3rdparty/bes/src clean

3rdparty:
	make -C 3rdparty/bes/src

$(TARGETS):
	make -f target/$@

%_clean:
	make -f target/$* clean

test: all
	make -f tests/Makefile
	cd tests && ./pinttests

test_clean:
	make -f tests/Makefile clean

clean: $(addsuffix _clean,$(TARGETS)) test_clean 3rdparty_clean

apidoc:
	rm -f docs/api/*
	ocamldoc -sort -html -d docs/api -I bindings -I pintlib -I anlib -I phlib \
		-t "Pint OCaml API" \
		bindings/r.mli pintlib/*.mli anlib/*.mli phlib/*.mli *.mli


RELNAME=$(shell date -I)
RELBRANCH=master

PREFIX=/usr
DESTDIR=

%_install: %
	make -f target/$* PREFIX=$(PREFIX) DESTDIR=$(DESTDIR) install

misc_install:
	for i in $(MISC_TOOLS); do \
		install -m 0755 $$i $(DESTDIR)$(PREFIX)/bin; \
	done

install: $(addsuffix _install,$(TARGETS)) misc_install aspfiles_install

pre-release:
	sed -i 's/:.*##VERSION##/: "$(RELNAME)",##VERSION##/' setup.py

release:
	git commit -a -m "release $(RELNAME)"
	git tag $(RELNAME)

OSX_W=/tmp/osx-pint/pint-$(RELNAME)
OSX_PREFIX=/pint
OSX_SHARE=$(OSX_PREFIX)/share
OSX_ROOT=$(OSX_W)$(OSX_PREFIX)
OSX_BIN=$(OSX_ROOT)/bin
OSX_DMG=../pint-$(RELNAME).dmg


# should be called using ./dist/osx/do.sh
#dist-osx: $(OXS_BINS)
dist-osx:
	-rm -rf $(OSX_W)
	make $(OSX_TARGETS)
	make DESTDIR="$(OSX_W)" PREFIX="$(OSX_PREFIX)" PINT_SHARE_PATH="$(OSX_SHARE)" $(addsuffix _install,$(OSX_TARGETS)) misc_install aspfiles_install
	#install -d $(OSX_W_BIN)
	#install -m 755 $(OSX_BINS:%=bin/%) $(MISC_TOOLS) $(OSX_W_BIN)
	install -m 644 dist/osx/*.dylib $(OSX_BIN)
	install -m 755 dist/osx/clingo $(OSX_BIN)
	install -m 755 dist/osx/mole $(OSX_BIN)
	for i in $(OSX_TARGETS); do \
		install -m 755 -b -B .mac dist/osx/wrapper.sh $(OSX_BIN)/$$i; \
	done
	MISC_TOOLS="$(MISC_TOOLS)" ./dist/osx/gen_install.sh > $(OSX_ROOT)/install.sh
	chmod 755 $(OSX_ROOT)/install.sh
	install -d $(OSX_ROOT)/examples
	install -d $(OSX_ROOT)/examples/toys
	install -m 644 examples/*.* $(OSX_ROOT)/examples
	install -m 644 examples/toys/* $(OSX_ROOT)/examples/toys
	install -m 644 dist/osx/README $(OSX_ROOT)
	-rm -f $(OSX_DMG)
	hdiutil create -srcfolder $(OSX_W) -volname pint-$(RELNAME) -fs HFS+ $(OSX_DMG)
	-rm -rf $(OSX_W)

run-dist-deb-via-docker:
	docker run --rm --volume $$PWD:/wd --workdir /wd pauleve/pint make dist-deb-via-docker RELNAME=$(RELNAME)

dist-deb-via-docker:
	apt-get update && \
		apt-get install -y devscripts debhelper \
			ocaml ocaml-findlib \
			camlidl \
			libextlib-ocaml-dev \
			libfacile-ocaml-dev \
			r-mathlib
	make dist-pre-deb
	make dist-deb
	mv -v ../pint_$(RELNAME)_*.deb dist/

dist-pre-deb:
	DEBEMAIL="Loic Pauleve <loic.pauleve@ens-cachan.org>" debchange -v $(RELNAME) Release $(RELNAME)
	DEBEMAIL="Loic Pauleve <loic.pauleve@ens-cachan.org>" debchange -r -D unstable ""

dist-deb:
	dpkg-buildpackage -tc

dist-docker:
	docker build -t pauleve/pint:latest -t pauleve/pint:$(RELNAME) --build-arg PINT_VERSION=$(RELNAME) .

dist-docker-publish:
	docker push pauleve/pint

dist: pre-release
	make run-dist-deb-via-docker
	make dist-docker

dist-publish: dist-docker-publish

