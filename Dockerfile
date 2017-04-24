FROM ubuntu:latest
MAINTAINER Pauleve Loic <loic.pauleve@lri.fr>
WORKDIR /notebook
ENTRYPOINT ["tini", "--"]
CMD ["pint-nb", "--NotebookApp.token="]
EXPOSE 8888

ENV TINI_VERSION 0.13.1
RUN apt-get update \
    && apt-get install -y \
        gringo \
        libgmpxx4ldbl \
        openjdk-8-jre-headless \
        python3-pandas \
        python3-pip \
        python3-pygraphviz \
        r-mathlib \
    && apt-get clean \
    && pip3 install jupyter \
    && cd /usr/src \
    && curl -LO https://github.com/krallin/tini/releases/download/v${TINI_VERSION}/tini_${TINI_VERSION}-amd64.deb \
    && dpkg -i tini_${TINI_VERSION}-amd64.deb \
    && rm *.deb

ARG PINT_VERSION
COPY dist/pint_${PINT_VERSION}_amd64.deb /usr/src
COPY notebook /notebook
COPY interfaces/ipython /usr/src/pypint
RUN dpkg -i /usr/src/pint_${PINT_VERSION}_amd64.deb \
    && pip3 install \
        boolean.py \
        /usr/src/pypint \
    && rm -rf /usr/src \
    && echo '#!/bin/bash' > /usr/bin/pint-nb \
    && echo 'jupyter notebook --allow-root --no-browser --ip=* --port 8888 "${@}"' >>/usr/bin/pint-nb \
    && chmod +x /usr/bin/pint-nb

