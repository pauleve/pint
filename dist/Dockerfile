FROM ocaml/opam:ubuntu-16.04_ocaml-4.05.0_flambda
MAINTAINER Pauleve Loic <loic.pauleve@lri.fr>

USER root
RUN apt-get update \
    && apt-get install -y \
        devscripts \
        debhelper

#RUN apt-get install -y --no-install-recommands \
#        r-mathlib

USER opam
RUN opam install extlib camlidl facile

