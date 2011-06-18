DISTNAME = inter
BINFILES = tester
SBINFILES =
LIBFILES =
DOCFILES = COPYING LICENSE CHANGELOG VERSION 
OTHERFILES = $(DISTNAME).spec
SRCFILES = $(shell echo *.ml *.mli *.mll *.mly) Makefile Makefile.ocaml
DISTFILES = $(DOCFILES) $(OTHERFILES) $(SRCFILES)
DISTVERSIONFILES =
MODULES = cards printer parser inter cofu
LIBRARIES = str unix
DISTVERSION = $(shell cat VERSION)
#VARIANT=.profile

LIBDIRS = 
BUILDSUBDIRS = 
REBUILDSUBDIRS = 

INSTALL_PREFIX = /

include Makefile.ocaml$(VARIANT)

