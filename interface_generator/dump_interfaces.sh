#!/usr/bin/env bash
set -e

for ocaml_version in 4.08.0; do \
#    3.11.2 3.12.1 4.00.1 \
#    4.01.0 4.02.3 4.03.0 4.04.2 4.05.0 4.06.1 \
#    4.07.0 4.08.0;
  target_dir=../interfaces/${ocaml_version:0:4}
  mkdir -p $target_dir
  for module in Fun; do
#      Pervasives Arg Array ArrayLabels Bool Buffer Bytes BytesLabels Callback Char \
#      Complex Digest Ephemeron Filename Float Format Fun Gc Genlex Hashtbl Int32 \
#      Int64 Lazy Lexing List ListLabels Map Marshal MoreLabels Nativeint Obj \
#      Oo Option Parsing Printexc Printf Queue Random Result Scanf Seq Set \
#      Spacetime Stack StdLabels Stream String StringLabels Sys Uchar Weak; do
    target=$target_dir/`echo ${module:0:1} | tr A-Z a-z`${module:1}.mli
    if [ "$ocaml_version" = "4.08.0" ]; then
        ./interface_dumper $module ~/tmp/ocaml-trunk/bin/ocaml >$target
    else
        opam config exec --switch=$ocaml_version -- \
             ./interface_dumper $module ocaml >$target
    fi
    [ `stat --format="%s" $target` -gt 1 ] || rm $target
#    [ `stat -f%z $target` -gt 1 ] || rm $target
  done
done
