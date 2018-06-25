ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
include Makefile.config
endif
endif

ifeq ($(OCAMLC),)
	OCAMLC_AVAILABLE := false
else
	OCAMLC_AVAILABLE := true
endif

ifeq ($(OCAMLOPT),)
	OCAMLOPT_AVAILABLE := false
else
	OCAMLOPT_AVAILABLE := true
endif

OCAMLFLAGS += $(addprefix -package ,$(RESULT_PKG) $(SEQ_PKG) $(UCHAR_PKG))

ifneq ($(OCAMLFIND),)
	OCAMLFLAGS_TESTS += -linkpkg
endif

MODULES := stdcompat__init stdcompat__root stdcompat__seq \
	stdcompat__tools		\
	stdcompat__pervasives stdcompat__arg stdcompat__lazy		\
	stdcompat__char stdcompat__uchar stdcompat__buffer		\
	stdcompat__string stdcompat__stringLabels stdcompat__bytes	\
	stdcompat__bytesLabels stdcompat__list stdcompat__listLabels	\
	stdcompat__stack stdcompat_hashtbl_ext \
	stdcompat__hashtbl stdcompat__set		\
	stdcompat__map stdcompat__weak stdcompat__sys			\
	stdcompat__stream stdcompat__digest stdcompat__nativeint	\
	stdcompat__int32 stdcompat__int64 stdcompat__filename		\
	stdcompat__array stdcompat__arrayLabels	\
	stdcompat__float stdcompat__queue stdcompat__ephemeron \
	stdcompat__spacetime

.PHONY : all
all : \
	$(patsubst %,bytecode,$(filter true,$(OCAMLC_AVAILABLE))) \
	$(patsubst %,native,$(filter true,$(OCAMLOPT_AVAILABLE))) doc

.PHONY : bytecode
bytecode : stdcompat.cma

.PHONY : native
native : stdcompat.cmxa stdcompat.cmxs

.PHONY : clean
clean :
	rm -f META $(foreach module, $(MODULES) stdcompat__native stdcompat, \
			$(foreach ext, .cmi .cmt .cmti .cmo .cmx .o .a, \
				$(module)$(ext))) \
		stdcompat.cma stdcompat.cmxs stdcompat.cmxa stdcompat.a \
		stdcompat_tests.cmt stdcompat_tests.cmti \
		stdcompat_tests.cmi stdcompat_tests.cmo .depend

.PHONY : distclean
distclean : clean
	rm -f $(foreach module, $(MODULES), \
			$(patsubst %.ml.in, %.ml, $(wildcard $(module).ml.in)) \
			$(patsubst %.mli.in, %.mli, \
				$(wildcard $(module).mli.in)))

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
ifneq ($(MAKECMDGOALS),distclean)
include .depend
endif
endif

stdcompat__list.cmo stdcompat__list.cmx : stdcompat__list.cmi

doc :
	mkdir -p doc
	$(OCAMLDOC) $(OCAMLFLAGS) -html -d $@ $^
	touch doc

%.cmi : %.mli
	$(OCAMLBEST) $(OCAMLFLAGS) -c $<

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

interface_generator : interface_generator.ml
	eval `opam config env --switch=4.07.0+rc1` && \
	ocamlfind ocamlc -g -package unix,compiler-libs.common -linkpkg $< -o $@

define generate_interface
stdcompat__$(target).mlip : interface_generator
	./interface_generator $(module) \
		~/.opam/4.07.0+rc1/bin/ocaml \
		~/.opam/4.06.1/bin/ocaml \
		~/.opam/4.05.0/bin/ocaml \
		~/.opam/4.04.2/bin/ocaml \
		~/.opam/4.03.0/bin/ocaml \
		~/.opam/4.02.3/bin/ocaml \
		~/.opam/4.01.0/bin/ocaml \
		~/.opam/4.00.1/bin/ocaml \
		~/.opam/3.12.1/bin/ocaml \
		~/.opam/3.11.2/bin/ocaml >$$@
endef

module:=Pervasives
target:=pervasives
$(eval $(generate_interface))

module:=List
target:=list
$(eval $(generate_interface))

module:=ListLabels
target:=listLabels
$(eval $(generate_interface))

module:=Sys
target:=sys
$(eval $(generate_interface))

module:=Array
target:=array
$(eval $(generate_interface))

module:=ArrayLabels
target:=arrayLabels
$(eval $(generate_interface))

module:=Stack
target:=stack
$(eval $(generate_interface))

module:=String
target:=string
$(eval $(generate_interface))

module:=StringLabels
target:=stringLabels
$(eval $(generate_interface))

module:=Char
target:=char
$(eval $(generate_interface))

module:=Digest
target:=digest
$(eval $(generate_interface))

module:=Filename
target:=filename
$(eval $(generate_interface))

module:=Queue
target:=queue
$(eval $(generate_interface))

module:=Arg
target:=arg
$(eval $(generate_interface))

module:=Lazy
target:=lazy
$(eval $(generate_interface))

module:=Buffer
target:=buffer
$(eval $(generate_interface))

module:=Stream
target:=stream
$(eval $(generate_interface))

module:=Nativeint
target:=nativeint
$(eval $(generate_interface))

module:=Int32
target:=int32
$(eval $(generate_interface))

module:=Int64
target:=int64
$(eval $(generate_interface))

module:=Bytes
target:=bytes
$(eval $(generate_interface))

module:=BytesLabels
target:=bytesLabels
$(eval $(generate_interface))

module:=Uchar
target:=uchar
$(eval $(generate_interface))

module:=Ephemeron
target:=ephemeron
$(eval $(generate_interface))

module:=Spacetime
target:=spacetime
$(eval $(generate_interface))

module:=Weak
target:=weak
$(eval $(generate_interface))

module:=Set
target:=set
$(eval $(generate_interface))

module:=Map
target:=map
$(eval $(generate_interface))

module:=Hashtbl
target:=hashtbl
$(eval $(generate_interface))

module:=Seq
target:=seq
$(eval $(generate_interface))

module:=Float
target:=float
$(eval $(generate_interface))
