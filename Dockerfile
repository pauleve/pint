FROM ubuntu:latest
MAINTAINER Pauleve Loic <loic.pauleve@lri.fr>

RUN apt-get update \
	&& apt-get install -y \
		r-mathlib \
		gringo \
		libgmpxx4ldbl \
		openjdk-8-jre-headless \
		python3-pip \
	&& apt-get clean

ENV TINI_VERSION 0.13.1
ADD https://github.com/krallin/tini/releases/download/v${TINI_VERSION}/tini_${TINI_VERSION}-amd64.deb /usr/src
RUN dpkg -i /usr/src/tini_${TINI_VERSION}-amd64.deb \
	&& pip3 install jupyter networkx pydotplus \
	&& echo '#!/bin/bash' > /usr/bin/pint-nb \
	&& echo 'jupyter notebook --no-browser --ip=* --port 8888 "${@}"' >>/usr/bin/pint-nb \
	&& chmod +x /usr/bin/pint-nb \
	&& mkdir -p ~/.jupyter \
	&& echo 'from IPython.lib import passwd' > ~/.jupyter/jupyter_notebook_config.py\
	&& echo 'get_config().NotebookApp.password = passwd("notebook")' >> ~/.jupyter/jupyter_notebook_config.py

ARG PINT_VERSION
ADD dist/pint_${PINT_VERSION}_amd64.deb /usr/src
ADD interfaces/ipython /usr/src/pint
RUN dpkg -i /usr/src/pint_${PINT_VERSION}_amd64.deb \
    && pip3 install /usr/src/pint && rm -rf /usr/src/pint \
	&& rm /usr/src/*.deb /usr/src/*.gz

ADD notebook /notebook
WORKDIR /notebook
ENTRYPOINT ["tini", "--"]
CMD ["pint-nb"]
EXPOSE 8888
