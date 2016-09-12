FROM ubuntu:latest
MAINTAINER Pauleve Loic <loic.pauleve@lri.fr>
ARG PINT_VERSION
ADD http://nusmv.fbk.eu/distrib/NuSMV-2.6.0-linux64.tar.gz /usr/src
RUN tar xvfz /usr/src/NuSMV-2.6.0-linux64.tar.gz -C /usr/src && ln -s /usr/src/NuSMV-2.6.0-Linux/bin/NuSMV /usr/bin/
ADD https://teamcity-systeme.lip6.fr/guestAuth/repository/download/bt54/.lastSuccessful/ITS_linux_64.tar.gz /usr/src
RUN mkdir /usr/src/its \
	&& tar xvfz /usr/src/ITS_linux_64.tar.gz -C /usr/src/its \
	&& ln -s /usr/src/its/its-reach /usr/bin \
	&& ln -s /usr/src/its/its-ctl /usr/bin \
	&& ln -s /usr/src/its/its-ltl /usr/bin
ADD https://github.com/pauleve/pint/releases/download/${PINT_VERSION}/pint_${PINT_VERSION}_amd64.deb /usr/src
RUN apt-get update \
	&& apt-get install -y \
		r-mathlib \
		gringo \
		libgmpxx4ldbl \
		maven \
	&& dpkg -i /usr/src/pint_${PINT_VERSION}_amd64.deb \
	&& rm -rf /usr/src/*.deb /var/lib/apt/lists/*
ADD http://www.lsv.ens-cachan.fr/~schwoon/tools/mole/mole-140428.tar.gz /usr/src
RUN tar xvfz /usr/src/mole-140428.tar.gz -C /usr/src \
	&& make -C /usr/src/mole-140428 \
	&& mv /usr/src/mole-140428/mole /usr/bin \
	&& mv /usr/src/mole-140428/mci2dot /usr/bin \
	&& rm -rf /usr/src/mole-140428
#RUN cd /usr/src \
#	&& git clone https://github.com/colomoto/logicalmodel.git \
#	&& cd logicalmodel && mvn package \
#	&& echo "#!/bin/sh\\java -jar $PWD/target/LogicalModel.jar \"${@}\"" >> /usr/bin/logicalmodel \
#	&& chmod +x /usr/bin/logicalmodel

