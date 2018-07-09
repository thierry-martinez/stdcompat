ACLOCAL_AMFLAGS = -I m4

PACKAGE=stdcompat
mypkgdir=$(pkglibdir)

if OCAML_SUPPORTS_SHARED
mypkg_SCRIPTS = \
	META stdcompat.cmi stdcompat.cma stdcompat.cmxa stdcompat.cmxs
else
mypkg_SCRIPTS = \
	META stdcompat.cmi stdcompat.cma stdcompat.cmxa
endif

PACKAGES = $(RESULT_PKG) $(SEQ_PKG) $(UCHAR_PKG)
OCAMLFLAGS = $(PACKAGES:%=-package %)
OCAMLCFLAGS = $(OCAMLFLAGS)
OCAMLLDFLAGS = $(OCAMLFLAGS)

SUFFIXES = .ml .mli .cmi .cmo .cmx .cmxa .cmxs

MODULES = stdcompat__init.ml stdcompat__root.ml stdcompat__seq.ml \
	stdcompat__tools.ml \
	stdcompat__pervasives.ml stdcompat__arg.ml stdcompat__lazy.ml \
	stdcompat__char.ml stdcompat__uchar.ml stdcompat__buffer.ml \
	stdcompat__string.ml stdcompat__stringLabels.ml stdcompat__bytes.ml \
	stdcompat__bytesLabels.ml stdcompat__list.ml stdcompat__listLabels.ml \
	stdcompat__stack.ml stdcompat__hashtbl_ext.ml \
	stdcompat__hashtbl.ml stdcompat__set.ml \
	stdcompat__map.ml stdcompat__weak.ml stdcompat__sys.ml \
	stdcompat__stream.ml stdcompat__digest.ml stdcompat__nativeint.ml \
	stdcompat__int32.ml stdcompat__int64.ml stdcompat__filename.ml \
	stdcompat__array.ml stdcompat__arrayLabels.ml \
	stdcompat__float.ml stdcompat__queue.ml stdcompat__ephemeron.ml \
	stdcompat__spacetime.ml

.DEFAULT_GOAL = all

.mli.cmi :
	$(OCAMLBEST) -c $< -o $@

.ml.cmo :
	$(OCAMLC) -c $< -o $@

.ml.cmx :
	$(OCAMLOPT) -c $< -o $@

stdcompat.cmi : stdcompat.cmo

stdcompat.cma : stdcompat__native.cmo $(MODULES:.ml=.cmo)
	$(OCAMLC) $(OCAMLLDFLAGS) -a $^ -o $@

stdcompat.cmxa : stdcompat__native.cmx $(MODULES:.ml=.cmx)
	$(OCAMLOPT) $(OCAMLLDFLAGS) -a $^ -o $@

stdcompat.cmxs : stdcompat__native.cmx $(MODULES:.ml=.cmx)
	$(OCAMLOPT) $(OCAMLLDFLAGS) -shared $^ -o $@

stdcompat__native.cmo : stdcompat__native.ml_byte
	cp stdcompat__native.ml_byte stdcompat__native.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c stdcompat__native.ml

stdcompat__native.cmx : stdcompat__native.ml_native stdcompat__native.cmo
	cp stdcompat__native.ml_native stdcompat__native.ml
	$(OCAMLOPT) $(OCAMLCFLAGS) -c stdcompat__native.ml

.PHONY : depend
depend : .depend

.depend : $(MODULES) $(MODULES:.ml=.mli) stdcompat.ml
	cp stdcompat__native.ml_byte stdcompat__native.ml
	$(OCAMLDEP) stdcompat__native.ml $^ >$@ || rm -f .depend

@INCLUDE_DEPEND@

CLEANFILES = *.cmi *.cmo $(bin_SCRIPTS) *~
