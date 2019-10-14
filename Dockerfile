FROM ubuntu:xenial
MAINTAINER "Nathan Bedell" "nbedell@tulane.edu"
CMD apt -y update
CMD apt -y install git \
                   software-properties-common
CMD add-apt-repository -y ppa:hvr/ghc
CMD apt update
CMD apt -y install ghc-8.4.4
CMD apt -y install cabal-install
CMD cabal update
WORKDIR /root
CMD git clone https://github.com/Sintrastes/bli-prolog
WORKDIR /root/bli-prolog
CMD cabal install