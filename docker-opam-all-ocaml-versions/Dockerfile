FROM ubuntu:16.04
RUN apt update
RUN echo yes | apt install autoconf automake unzip aspcud rsync git mercurial \
    darcs wget build-essential sudo vim
RUN useradd -m -s /bin/bash ci
RUN echo ci      ALL=\(ALL\) NOPASSWD:ALL >/etc/sudoers
USER ci
RUN wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - \
    | sh -s /usr/local/bin
RUN opam update && opam switch 3.07
RUN opam update && opam switch 3.08.4
RUN opam update && opam switch 3.09.3
RUN opam update && opam switch 3.10.2
RUN opam update && opam switch 3.11.2
RUN opam update && opam switch 3.12.1
RUN opam update && opam switch 4.00.1
RUN opam update && opam switch 4.01.0
RUN opam update && opam switch 4.02.3
RUN opam update && opam switch 4.03.0
RUN opam update && opam switch 4.04.2
RUN opam update && opam switch 4.05.0
RUN opam update && opam switch 4.06.0
RUN opam update && opam switch 4.07.0