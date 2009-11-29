OCAMLMAKEFILE = ../../OCamlMakefile

SOURCES		= xmlstring.ml
PACKS		= ulex xml
OCAMLFLAGS	= -syntax camlp4o
OCAMLDEP	= ocamldep -package ulex -syntax camlp4o
RESULT		= xmlstring

include ../../Makefile.global
include ../Makefile.inc

all: ncl bcl

include $(OCAMLMAKEFILE)

