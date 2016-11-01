#!/bin/sh
VERSION=0.9.3
DIR=/var/lib/gforge/chroot/home/groups/bes/htdocs
HOST=bes.forge.ocamlcore.org
USER=mww
BASE=$(pwd)
cd ${BASE} && \
   make doc && \
cd ${BASE}/doclib.docdir && \
  ssh ${USER}@${HOST} 'rm -f ${DIR}/doc/*' && \
  scp *.html *.css ${USER}@${HOST}:${DIR}/doc/ && \
cd ${BASE}/www && \
  make index.html && \
  scp index.html ${USER}@${HOST}:${DIR}/
cd ${BASE} && \
  mkdir bes-${VERSION} && \
  cp -rv AUTHORS.txt INSTALL.txt Makefile README.txt _oasis _tags configure myocamlbuild.ml setup.ml src bes-${VERSION} && \
  tar --exclude .svn -cvzf bes-${VERSION}.tar.gz bes-${VERSION} && \
  rm -rf bes-${VERSION}

