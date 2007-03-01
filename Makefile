PROJECTNAME = xmlm
VERSION = 0.9.1
COPYRIGHTYEAR = 2007

# Compiler options
OCAMLCFLAGS = -dtypes -I src
OCAMLOPTFLAGS = -I src

INSTALLDIR = $(shell $(OCAMLC) -where)/$(PROJECTNAME)
DOCDIR = doc

# Library
libsources = src/xmlm.ml

# Test
testsources = test/xmltrip.ml
testname = test/xmltrip

default: lib test

lib: $(libsources:.ml=.cmi) $(libsources:.ml=.cmo) $(libsources:.ml=.cmx)

test: $(testname) $(testname).opt

$(testname): $(libsources:.ml=.cmo) $(testsources:.ml=.cmo)
	$(OCAMLC) $(OCAMLCFLAGS) -o $@ $+

$(testname).opt: $(libsources:.ml=.cmx) $(testsources:.ml=.cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ $+

install: lib
	$(MKDIR) -p $(INSTALLDIR)
	$(CP) $(libsources) $(libsources:.ml=.mli) $(libsources:.ml=.cmi) \
	$(libsources:.ml=.cmx) $(libsources:.ml=.o) $(INSTALLDIR)

doc: $(libsources:.ml=.cmi)
	$(OCAMLDOC) -I . -html -colorize-code -d $(DOCDIR) \
	$(libsources:.ml=.mli)

clean:
	$(RM) -f $(libsources:.ml=.cmo) $(libsources:.ml=.cmx) \
	$(libsources:.ml=.cmi) $(libsources:.ml=.o) $(libsources:.ml=.annot) \
	$(testsources:.ml=.cmo) $(testsources:.ml=.cmx) \
	$(testsources:.ml=.cmi) $(testsources:.ml=.annot) $(testsources:.ml=.o)\
	$(testname) $(testname).opt

clean-all: clean
	$(RM) -f $(DOCDIR)/*.html

depend:
	$(OCAMLDEP) $(sources:.ml=.mli) $(sources) > .depend

-include .depend

DIRNAME = $(PROJECTNAME)-$(VERSION)
ROOTDIR = /tmp/$(DIRNAME)
distrib: 
	$(RM) -rf $(ROOTDIR)
	$(MKDIR) -p $(ROOTDIR)
	$(CP) -r . $(ROOTDIR)
	$(RM) -rf $(ROOTDIR)/_darcs
	$(MAKE) -C $(ROOTDIR) clean-all

	$(FIND) $(ROOTDIR) \
	\( -name "*~" -o -name ".DS_Store" -o -name ".gdb_history" \) \
	-exec $(RM) {} ';'

	for file in `$(FIND) $(ROOTDIR) -type f -print`; \
	do sed \
	"/%%VERSION%%/s//$(VERSION)/; /%%COPYRIGHTYEAR%%/s//$(COPYRIGHTYEAR)/" \
	$$file > $$file.tmp && mv -f $$file.tmp $$file; \
	done

	$(MAKE) -C $(ROOTDIR) depend default test doc
	$(MAKE) -C $(ROOTDIR) clean
	$(CD) $(ROOTDIR)/.. && $(TAR) -cvjf $(DIRNAME).tbz $(DIRNAME)
	$(RM) -r $(ROOTDIR)

.PHONY : default lib test install doc clean clean-all depend distrib

# Ocaml tools
OCAMLBINPREFIX =
OPT = .opt
OCAMLC = $(OCAMLBINPREFIX)ocamlc$(OPT)
OCAMLOPT = $(OCAMLBINPREFIX)ocamlopt$(OPT)
OCAMLDEP = $(OCAMLBINPREFIX)ocamldep$(OPT)
OCAMLDOC = $(OCAMLBINPREFIX)ocamldoc$(OPT)

# Misc
SHELL = /bin/sh
RM = rm
CP = cp
CD = cd
MKDIR = mkdir
TOUCH = touch
TAR = gnutar
FIND = find

# pattern rules
%.cmo : %.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c $<

%.cmi : %.mli
	$(OCAMLC) $(OCAMLCFLAGS) -c $<

%.cmi : %.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c $<

%.cmx : %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $< 
