FROM ubuntu:focal

RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y \
  git python3.8 wget build-essential emacs ruby rhino python3-pip
RUN pip3 install afdko
RUN PERL_MM_USE_DEFAULT=1 cpan App::cpanminus
RUN cpanm Math::Clipper
RUN git clone https://github.com/adobe-type-tools/perl-scripts.git /opt/perl-scripts
