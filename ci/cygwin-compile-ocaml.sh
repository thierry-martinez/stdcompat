#!/bin/bash
set -e -x
VERSION="$1"
URL="http://caml.inria.fr/pub/distrib/ocaml-${VERSION:0:4}/ocaml-$VERSION.tar.gz"
if [ ! -d "/cygdrive/c/ocaml/$VERSION/" ]; then
    wget "$URL"
    tar --extract --file="ocaml-$VERSION.tar.gz"
    cd "ocaml-$VERSION"
    pushd flexdll
      wget https://github.com/alainfrisch/flexdll/archive/0.37.tar.gz
      tar --strip-components=1 --extract --file=0.37.tar.gz
      popd
    ./configure --build=x86_64-unknown-cygwin --host=x86_64-pc-windows \
      --prefix="C:/ocaml/$VERSION"
    make flexdll
    make world.opt
    make flexlink.opt
    make install
fi
