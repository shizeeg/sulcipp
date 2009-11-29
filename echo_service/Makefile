OCAMLMAKEFILE = ../OCamlMakefile

include ../Makefile.global

VERSION		= 0.1-20051010
SOURCES		= version.ml echo_service.ml
PRE_TARGETS	= version.ml
PACKS		= service
RESULT		= echo_service
TRASH		= version.ml

OCAMLLDFLAGS	= -verbose

all: nc

test:
	echo $(OCAMLPATH)
version.ml:
	echo "let name = \"Stoat Echo Service\"" > version.ml
	echo "let version = \"$(VERSION)\"" >> version.ml

include $(OCAMLMAKEFILE)

