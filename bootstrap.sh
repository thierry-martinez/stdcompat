#!/bin/sh

if test ! -x configure; then
    if ! autoconf; then
        echo bootstrap.sh: autoconf failed. >&2
        exit 1
    fi
fi
