OCAMLMAKEFILE = ../../OCamlMakefile

SOURCES		= xmlstring_netstring.ml
PACKS		= ulex netstring xmlstring
OCAMLFLAGS	= -syntax camlp4o
OCAMLDEP	= ocamldep -package ulex -syntax camlp4o
RESULT		= xmlstring_netstring

include ../../Makefile.global
include ../Makefile.inc

all: ncl bcl

include $(OCAMLMAKEFILE)

