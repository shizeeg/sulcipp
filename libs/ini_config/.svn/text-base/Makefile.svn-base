OCAMLMAKEFILE = ../../OCamlMakefile

SOURCES = ini_parser.mly ini_lexer.mll ini_config.ml
RESULT = ini_config

include ../../Makefile.global
include ../Makefile.inc
LIBINSTALL_FILES += ini_config.cmi

all: bcl ncl

include $(OCAMLMAKEFILE)

