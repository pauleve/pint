FROM ubuntu:latest
MAINTAINER Pauleve Loic <loic.pauleve@lri.fr>
ADD http://nusmv.fbk.eu/distrib/NuSMV-2.6.0-linux64.tar.gz /usr/src
RUN tar xvfz /usr/src/NuSMV-2.6.0-linux64.tar.gz -C /usr/src && ln -s /usr/src/NuSMV-2.6.0-Linux/bin/NuSMV /usr/bin/
RUN apt-get update \
	&& apt-get install -y \
		r-mathlib \
		gringo \
		libgmpxx4ldbl \
		maven \
		git \
		openjdk-8-jdk
ADD http://www.lsv.ens-cachan.fr/~schwoon/tools/mole/mole-140428.tar.gz /usr/src
RUN tar xvfz /usr/src/mole-140428.tar.gz -C /usr/src \
	&& make -C /usr/src/mole-140428 \
	&& mv /usr/src/mole-140428/mole /usr/bin \
	&& mv /usr/src/mole-140428/mci2dot /usr/bin \
	&& rm -rf /usr/src/mole-140428
ADD http://ginsim.org/sites/default/files/GINsim-2.9.4-with-deps.jar /usr/src
RUN echo '#!/bin/sh' > /usr/bin/GINsim \
	&& echo "java -jar /usr/src/GINsim-2.9.4-with-deps.jar \"${@}\"" >> /usr/bin/GINsim\
	&& chmod +x /usr/bin/GINsim
RUN git clone https://github.com/colomoto/logicalmodel.git /usr/src/logicalmodel\
	&& cd /usr/src/logicalmodel && mvn package \
	&& echo '#!/bin/sh' > /usr/bin/logicalmodel \
	&& echo "java -jar $PWD/target/LogicalModel-0.3-SNAPSHOT.jar \"\${@}\"" >> /usr/bin/logicalmodel \
	&& chmod +x /usr/bin/logicalmodel \
	&& rm -rf ~/.m2
ADD https://teamcity-systeme.lip6.fr/guestAuth/repository/download/bt54/.lastSuccessful/ITS_linux_64.tar.gz /usr/src
RUN mkdir /usr/src/its \
	&& tar xvfz /usr/src/ITS_linux_64.tar.gz -C /usr/src/its \
	&& ln -s /usr/src/its/its-reach /usr/bin \
	&& ln -s /usr/src/its/its-ctl /usr/bin \
	&& ln -s /usr/src/its/its-ltl /usr/bin
ARG PINT_VERSION
ADD dist/pint_${PINT_VERSION}_amd64.deb /usr/src
RUN dpkg -i /usr/src/pint_${PINT_VERSION}_amd64.deb \
	&& rm /usr/src/*.deb /usr/src/*.gz

