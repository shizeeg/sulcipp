OCAMLMAKEFILE = ../../OCamlMakefile

LOG_FLAGS	= -log_all
SOURCES		= common.ml config.ml translator.ml service.ml
PACKS		= unix xmpp getopt logger xmlstring
OCAMLDEP	= ocamldep -package logger -syntax camlp4o -ppopt $(LOG_FLAGS)
OCAMLFLAGS	= -syntax camlp4o -ppopt $(LOG_FLAGS)
RESULT		= service

include ../../Makefile.global
include ../Makefile.inc

LIBINSTALL_FILES += config.cmi common.cmi translator.cmi

all: ncl bcl

include $(OCAMLMAKEFILE)

