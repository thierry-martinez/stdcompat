``Stdcompat``: compatibility module for OCaml standard library
==============================================================

``Stdcompat`` is a compatibility layer allowing programs to use some
recent additions to the OCaml standard library while preserving the
ability to be compiled on former versions of OCaml.

The ``Stdcompat`` API is not intended to be stable, but there will be
efforts to allow future versions of ``Stdcompat`` to be compiled on a
large range of versions of OCaml: ``Stdcompat`` should compile (at least)
on every version of OCaml following 3.12.0 (included).

The module ``Stdcompat`` provides some definitions for values and
types introduced in recent versions of the standard library. These
definitions are just alias to the matching definition of the standard
library if the latter is recent enough. Otherwise, the module
``Stdcompat`` provides an alternative implementation.
