#!/usr/bin/env bash
set -e

    #3.11.2 3.12.1 4.00.1 \
    #4.01.0 4.02.3 4.03.0 4.04.2 4.05.0 4.06.1 \
    #4.07.0
for ocaml_version in 4.08.0+trunk; do
  target_dir=../interfaces/${ocaml_version:0:4}
  mkdir -p $target_dir
  for module in \
      Pervasives Arg Array ArrayLabels Buffer Bytes BytesLabels Callback Char \
      Complex Digest Ephemeron Filename Float Format Gc Genlex Hashtbl Int32 \
      Int64 Lazy Lexing List ListLabels Map Marshal MoreLabels Nativeint Obj \
      Oo Option Parsing Printexc Printf Queue Random Result Scanf Seq Set Sort \
      Spacetime Stack StdLabels Stream String StringLabels Sys Uchar Weak; do
    target=$target_dir/`echo ${module:0:1} | tr A-Z a-z`${module:1}.mli
    opam config exec --switch=$ocaml_version -- \
      ./interface_dumper $module ocaml >$target
    [ `stat --format="%s" $target` -gt 1 ] || rm $target
  done
done
