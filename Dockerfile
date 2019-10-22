FROM ubuntu
MAINTAINER "Nathan Bedell" "nbedell@tulane.edu"
RUN apt -y update
RUN apt -y install git \
                   zlib1g-dev \
                   software-properties-common
RUN add-apt-repository -y ppa:hvr/ghc
RUN apt update
RUN apt -y install ghc-8.4.4
RUN apt -y install cabal-install
RUN cabal update
WORKDIR /root
RUN git clone https://github.com/Sintrastes/bli-prolog
WORKDIR /root/bli-prolog
RUN cabal install