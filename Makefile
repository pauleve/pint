
TARGETS=\
	libpint\
	pinttop\
	pint-config\
	pint-export\
	pint-lcg\
	pint-mole\
	pint-reach\
	pint-sg\
	pint-stable\
	#phc\
	ph-exec\
	ph2thomas\

OSX_TARGETS=\
	pint-config\
	pint-export\
	pint-lcg\
	pint-mole\
	pint-reach\
	pint-sg\
	pint-stable\
	#phc\
	ph-exec\
	ph2thomas\


MISC_TOOLS = \
	bin/pint_install_deps

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

dist-clean: clean
	rm -rf build

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

website:
	make -C docs html

pre-release:
	sed -i 's/=.*(\*VERSION\*)/= "$(RELNAME)" (\*VERSION\*)/' pintlib/pintmeta.ml
	sed -i 's/set version = [^ ]*/set version = "$(RELNAME)"/' dist/conda/pint/meta.yaml
	sed -i 's/^release = [^ ]*/release = "$(RELNAME)"/' docs/src/conf.py

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
	make clean
	make 3rdparty
	make $(OSX_TARGETS)
	make DESTDIR="$(OSX_W)" PREFIX="$(OSX_PREFIX)" PINT_SHARE_PATH="$(OSX_SHARE)" $(addsuffix _install,$(OSX_TARGETS)) misc_install aspfiles_install
	install -m 644 dist/osx/*.dylib $(OSX_BIN)
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

DOCKER_BUILDER=pint-dist-builder

DOCKER=sudo docker

docker-dist-builder:
	$(DOCKER) pull $(shell grep FROM dist/Dockerfile|sed 's:FROM::')
	$(DOCKER) build -t $(DOCKER_BUILDER) dist

DOCKER_BUILDER_TARGETS=dist-pre-deb dist-deb dist-static

make-dist-via-docker: docker-dist-builder
	$(DOCKER) run --rm --volume $$PWD:/home/opam/pint-$(RELNAME) \
		--workdir /home/opam/pint-$(RELNAME) \
		$(DOCKER_BUILDER) \
		make RELNAME=$(RELNAME) $(DOCKER_BUILDER_TARGETS) dist-clean

dist-pre-deb:
	DEBEMAIL="Loic Pauleve <loic.pauleve@ens-cachan.org>" debchange -v $(RELNAME) Release $(RELNAME)
	DEBEMAIL="Loic Pauleve <loic.pauleve@ens-cachan.org>" debchange -r -D unstable ""

dist-deb:
	mk-build-deps -i -r -t "apt-get -y --no-install-recommends" debian/control
	dpkg-buildpackage -tc
	mv -v ../pint_$(RELNAME)_*.deb dist/

STATIC_BASENAME=pint_$(RELNAME)_linux-$(shell arch)
STATIC_DESTDIR=dist/$(STATIC_BASENAME)

dist-static: dist-clean
	python setup.py --disable-R --pint-env --share-path "/usr/local/share/pint"
	make OCAMLNLDFLAGS="-ccopt -static"
	make DESTDIR="$(STATIC_DESTDIR)" PREFIX="/" \
		PINT_SHARE_PATH="/share" \
		install
	install -m 755 dist/static/bin/pint-env $(STATIC_DESTDIR)/bin
	install -m 644 dist/static/README $(STATIC_DESTDIR)
	install -m 755 dist/static/install.sh $(STATIC_DESTDIR)
	tar cvJf $(STATIC_DESTDIR).txz -C dist $(STATIC_BASENAME)

dist: pre-release
	make make-dist-via-docker

dist-publish:

