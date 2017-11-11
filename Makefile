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
else
	OCAMLOPT := $(shell \
		if ocamlopt.opt -version >/dev/null 2>&1; then \
			echo ocamlopt.opt; \
		elif ocamlopt -version >/dev/null 2>&1; then \
			echo ocamlopt; \
		fi \
	)

	OCAMLC := $(shell \
		if ocamlc.opt -version >/dev/null 2>&1; then \
			echo ocamlc.opt; \
		elif ocamlc -version >/dev/null 2>&1; then \
			echo ocamlc; \
		fi \
	)

	OCAMLDOC := ocamldoc
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

OCAMLVERSION := $(shell $(OCAMLC) -version | cut -c 1,3-4)

CPPO := cppo
CPP := cpp

CPPO_AVAILABLE := $(shell \
	if $(CPPO) -version >/dev/null 2>&1; then \
		echo true; \
	else \
		echo false; \
	fi \
)

ifeq ($(CPPO_AVAILABLE),true)
	PP := $(CPPO) -D 'OCAMLVERSION $(OCAMLVERSION)'
else
	PP := $(CPP) -DOCAMLVERSION=$(OCAMLVERSION) -undef
endif

.PHONY : all
all : bytecode $(patsubst %,native,$(filter true,$(OCAMLOPT_AVAILABLE))) doc

.PHONY : bytecode
bytecode : stdcompat.cma

.PHONY : native
native : stdcompat.cmxa stdcompat.cmxs

.PHONY : clean
clean :
	rm -f stdcompat.ml stdcompat.mli \
		stdcompat.cmi \
		stdcompat.cmo stdcompat.cmx stdcompat.o \
		stdcompat.cma stdcompat.cmxs stdcompat.cmxa stdcompat.a

.PHONY : install
install : META stdcompat.cma stdcompat.cmi \
	$(patsubst %,stdcompat.cmxs stdcompat.cmxa stdcompat.a, \
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

stdcompat.ml stdcompat.mli: %: %p
	$(PP) $^ >$@ || rm $@

stdcompat.cmo stdcompat.cmx: stdcompat.cmi

doc: stdcompat.mli
	mkdir -p doc
	$(OCAMLDOC) -html -d $@ $^
	touch doc

%.cmi: %.mli
	$(OCAMLC) -c $<

%.cmo: %.ml
	$(OCAMLC) -c $<

%.cmx: %.ml
	$(OCAMLOPT) -c $<

stdcompat.cma: stdcompat.cmo
	$(OCAMLC) -a stdcompat.cmo -o $@

stdcompat.cmxa: stdcompat.cmx
	$(OCAMLOPT) -a stdcompat.cmx -o $@

stdcompat.cmxs: stdcompat.cmx
	$(OCAMLOPT) -shared stdcompat.cmx -o $@
