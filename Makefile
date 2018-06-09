USE_MAGIC := true
OCAMLFIND := ocamlfind

OCAMLFIND_AVAILABLE := $(shell \
	if $(OCAMLFIND) query -help >/dev/null 2>&1; then \
		echo true; \
	else \
		echo false; \
	fi \
)

ifeq ($(OCAMLFIND_AVAILABLE),true)
	OCAMLOPT := $(OCAMLFIND) ocamlopt
	OCAMLC := $(OCAMLFIND) ocamlc
	OCAMLDOC := $(OCAMLFIND) ocamldoc
	RESULT_PKG_AVAILABLE := $(shell \
	if $(OCAMLFIND) query result >/dev/null 2>&1; then \
		echo true; \
	else \
		echo false; \
	fi \
)
	SEQ_PKG_AVAILABLE := $(shell \
	if $(OCAMLFIND) query seq >/dev/null 2>&1; then \
		echo true; \
	else \
		echo false; \
	fi \
)
	UCHAR_PKG_AVAILABLE := $(shell \
	if $(OCAMLFIND) query uchar >/dev/null 2>&1; then \
		echo true; \
	else \
		echo false; \
	fi \
)
else
	OCAMLOPT := $(shell \
		if ocamlopt.opt -version >/dev/null 2>&1; then \
			echo ocamlopt.opt; \
		elif ocamlopt -version >/dev/null 2>&1; then \
			echo ocamlopt; \
		fi \
	) $(OCAMLFLAGS)

	OCAMLC := $(shell \
		if ocamlc.opt -version >/dev/null 2>&1; then \
			echo ocamlc.opt; \
		elif ocamlc -version >/dev/null 2>&1; then \
			echo ocamlc; \
		fi \
	) $(OCAMLFLAGS)

	OCAMLDOC := ocamldoc $(OCAMLFLAGS)

	RESULT_PKG_AVAILABLE := false
	SEQ_PKG_AVAILABLE := false
	UCHAR_PKG_AVAILABLE := false
endif

ifeq ($(OCAMLC),)
$(error There is no OCaml compiler available in PATH)
endif

ifeq ($(OCAMLOPT),)
	OCAMLOPT_AVAILABLE := false
else
	OCAMLOPT_AVAILABLE := $(shell \
		if $(OCAMLOPT) -version >/dev/null 2>&1; then \
			echo true; \
		else \
			echo false; \
		fi \
	)
endif

OCAML_VERSION := $(shell $(OCAMLC) -version | cut -c 1-6)

ifeq (4.07.0,$(word 1,$(sort 4.07.0 $(OCAML_VERSION))))
	USE_SEQ_PKG := false
else
	USE_SEQ_PKG := $(SEQ_PKG_AVAILABLE)
endif

ifeq (4.03.0,$(word 1,$(sort 4.03.0 $(OCAML_VERSION))))
	USE_RESULT_PKG := false
else
	USE_RESULT_PKG := $(RESULT_PKG_AVAILABLE)
endif

ifeq (4.03.0,$(word 1,$(sort 4.03.0 $(OCAML_VERSION))))
	USE_UCHAR_PKG := false
else
	USE_UCHAR_PKG := $(UCHAR_PKG_AVAILABLE)
endif

ifeq ($(USE_SEQ_PKG),true)
	OCAMLFLAGS += -package seq
endif

ifeq ($(USE_RESULT_PKG),true)
	OCAMLFLAGS += -package result
endif

ifeq ($(USE_UCHAR_PKG),true)
	OCAMLFLAGS += -package uchar
endif

ifeq ($(OCAMLFIND_AVAILABLE),true)
	OCAMLFLAGS_TESTS += -linkpkg
endif

CPPO := cppo
CPP := cpp

CPPO_AVAILABLE := $(shell \
	if $(CPPO) -version >/dev/null 2>&1; then \
		echo true; \
	else \
		echo false; \
	fi \
)

LITTLE_ENDIAN := $(shell \
	echo -n I | hexdump -o | awk '{ print substr($$2,6,1); exit }' \
)

ifeq ($(CPPO_AVAILABLE),true)
	PP = $(CPPO) -V 'OCAML:$(OCAML_VERSION)' $<
	PP_NATIVE_ARGS := -D OCAMLNATIVE
	ifeq ($(LITTLE_ENDIAN),0)
		PP += -D BIGENDIAN
	endif
	ifeq ($(USE_SEQ_PKG),true)
		PP += -D 'HAS_SEQ_PKG'
		REQUIRES += seq
	endif
	ifeq ($(USE_RESULT_PKG),true)
		PP += -D 'HAS_RESULT_PKG'
		REQUIRES += result
	endif
	ifeq ($(USE_UCHAR_PKG),true)
		PP += -D 'HAS_UCHAR_PKG'
		REQUIRES += uchar
	endif
	ifeq ($(USE_MAGIC),true)
		PP += -D 'USE_MAGIC'
	endif
else
	OCAML_VERSION_STRIPPED := $(subst .,,$(OCAML_VERSION))
	PP = <$< sed -e '/^\#/s/(\(.\),\(..\),\(.\))/\1\2\3/' | \
		$(CPP) -DOCAML_VERSION=$(OCAML_VERSION_STRIPPED) -undef -w -
	PP_NATIVE_ARGS := -DOCAMLNATIVE
	ifeq ($(LITTLE_ENDIAN),0)
		PP += -DBIGENDIAN
	endif
	ifeq ($(USE_SEQ_PKG),true)
		PP += -DHAS_SEQ_PKG
		REQUIRES += seq
	endif
	ifeq ($(USE_RESULT_PKG),true)
		PP += -DHAS_RESULT_PKG
		REQUIRES += result
	endif
	ifeq ($(USE_UCHAR_PKG),true)
		PP += -DHAS_UCHAR_PKG
		REQUIRES += uchar
	endif
	ifeq ($(USE_MAGIC),true)
		PP += -DUSE_MAGIC
	endif
endif

PP_INTF = $(PP)
PP_IMPL = $(PP)
PP_BYTECODE = $(PP)
PP_NATIVE = $(PP) $(PP_NATIVE_ARGS)

ifeq ($(CPPO_AVAILABLE),true)
  PP_META += $(PP) -D 'REQUIRES "$(REQUIRES)"'
else
  PP_META += $(PP) -D 'REQUIRES="$(REQUIRES)"'
endif

MODULES := stdcompat__root stdcompat__seq stdcompat__tools		\
	stdcompat__pervasives stdcompat__arg stdcompat__lazy		\
	stdcompat__char stdcompat__uchar stdcompat__buffer		\
	stdcompat__string stdcompat__stringLabels stdcompat__bytes	\
	stdcompat__bytesLabels stdcompat__list stdcompat__listLabels	\
	stdcompat__stack stdcompat__hashtbl stdcompat__set		\
	stdcompat__map stdcompat__weak stdcompat__sys			\
	stdcompat__stream stdcompat__digest stdcompat__nativeint	\
	stdcompat__int32 stdcompat__int64 stdcompat__filename		\
	stdcompat__array stdcompat__arrayLabels	\
	stdcompat__float stdcompat__queue stdcompat__ephemeron \
	stdcompat__spacetime

.PHONY : all
all : bytecode $(patsubst %,native,$(filter true,$(OCAMLOPT_AVAILABLE))) doc

.PHONY : bytecode
bytecode : stdcompat.cma

.PHONY : native
native : stdcompat.cmxa stdcompat.cmxs

.PHONY : clean
clean :
	rm -f META $(foreach module, $(MODULES) stdcompat__native stdcompat, \
			$(foreach ext, .cmi .cmt .cmti .cmo .cmx .o .a, \
				$(module)$(ext))) \
		$(foreach module, $(MODULES), \
			$(patsubst %.mlp, %.ml, $(wildcard $(module).mlp)) \
			$(patsubst %.mlip, %.mli, \
				$(wildcard $(module).mlip))) \
		stdcompat.cma stdcompat.cmxs stdcompat.cmxa stdcompat.a \
		stdcompat_tests.cmt stdcompat_tests.cmti \
		stdcompat_tests.cmi stdcompat_tests.cmo .depend

.PHONY : install
install : META stdcompat.cma stdcompat.cmxs stdcompat.cmxa stdcompat.a \
	$(patsubst %, \
	  $(foreach module,$(MODULES) stdcompat__native stdcompat, \
	    $(foreach ext,.cmi .cmx, \
	      $(module)$(ext))), \
	  $(filter true,$(OCAMLOPT_AVAILABLE)))
ifeq ($(HAVE_OCAMLFIND),no)
	$(error ocamlfind is needed for 'make install')
endif
	$(OCAMLFIND) install stdcompat $^

.PHONY : uninstall
uninstall :
ifeq ($(HAVE_OCAMLFIND),no)
	$(error ocamlfind is needed for 'make uninstall')
endif
	$(OCAMLFIND) remove stdcompat

.PHONY : depend
depend : .depend

ocaml_files := $(foreach module, $(MODULES), \
	    $(foreach ext, .ml .mli, \
	      $(module)$(ext))) stdcompat.ml

.depend : $(ocaml_files)
	ocamldep -impl stdcompat__native.ml_byte $(ocaml_files) >$@

ifneq ($(MAKECMDGOALS),clean)
include .depend
endif

stdcompat__list.cmo stdcompat__list.cmx : stdcompat__list.cmi

%.mli : %.mlip
	$(PP_INTF) >$@ || rm $@

%.ml : %.mlp
	$(PP_IMPL) >$@ || rm $@

doc :
	mkdir -p doc
	$(OCAMLDOC) $(OCAMLFLAGS) -html -d $@ $^
	touch doc

%.cmi : %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c $<

%.cmo : %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $<

%.cmx : %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

stdcompat__native.cmo : stdcompat__native.ml_byte
	$(OCAMLC) $(OCAMLFLAGS) -c -impl $<

stdcompat__native.cmx : stdcompat__native.ml_native
	$(OCAMLOPT) $(OCAMLFLAGS) -c -impl $<

stdcompat.cma : $(addsuffix .cmo, stdcompat__native $(MODULES) stdcompat)
	$(OCAMLC) -a $^ -o $@

stdcompat.cmxa : $(addsuffix .cmx, stdcompat__native $(MODULES) stdcompat)
	$(OCAMLOPT) -a $^ -o $@

stdcompat.cmxs : $(addsuffix .cmx, stdcompat__native $(MODULES) stdcompat)
	$(OCAMLOPT) -shared $^ -o $@

.PHONY : tests_bytecode
tests_bytecode : tests.bytecode
	./tests.bytecode

tests.bytecode : stdcompat.cma stdcompat_tests.cmo
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLFLAGS_TESTS) -o $@ $^

.PHONY : tests_native
tests_native : tests.native
	./tests.native

tests.native : stdcompat.cmxa stdcompat_tests.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) $(OCAMLFLAGS_TESTS) -o $@ $^

stdcompat_tests.cmo : stdcompat.cmi

stdcompat_tests.cmx : stdcompat.cmi

META : META.pp
	$(PP_META) >$@ || rm $@

interface_generator : interface_generator.ml
	ocamlfind ocamlopt -package unix,compiler-libs.common -linkpkg $< -o $@
