#!/usr/bin/env bash
set -e

for module in \
    Pervasives Arg Array ArrayLabels Buffer Bytes BytesLabels Callback Char \
    Complex Digest Ephemeron Filename Float Format Gc Genlex Hashtbl Int32 \
    Int64 Lazy Lexing List ListLabels Map Marshal MoreLabels Nativeint Obj Oo \
    Option Parsing Printexc Printf Queue Random Result Scanf Seq Set Spacetime \
    Stack StdLabels Stream String StringLabels Sys Uchar Weak; do
    target=../stdcompat__`echo $module | tr A-Z a-z`.mli.in
    echo $module
    ./interface_generator $module 4.08 4.07 4.06 4.05 4.04 4.03 4.02 4.01 4.00 3.12 \
       3.11 3.10 3.09 3.08 3.07 >$target
done
