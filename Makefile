PROJECTNAME = xmlm
VERSION = 0.9.0
COPYRIGHTYEAR = 2007

# Compiler options
OCAMLCFLAGS = -dtypes -g -I src
OCAMLOPTFLAGS = -I src

INSTALLDIR = $(shell $(OCAMLC) -where)/xmlm
HTMLDOCDIR = doc

# Source files
lib = src/xmlm
exec = test/xmltrip
sources = $(lib).ml $(exec).ml

default: $(lib).cmi $(lib).cmo $(lib).cmx

test: $(exec) $(exec).opt

$(exec): $(sources:.ml=.cmo)
	$(OCAMLC) $(OCAMLCFLAGS) -o $@ $+

$(exec).opt: $(sources:.ml=.cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ $+

install:
	$(MKDIR) -p $(INSTALLDIR)
	$(CP) $(lib).mli $(lib).ml $(lib).cmi $(lib).cmo $(lib).cmx \
	$(lib).o $(INSTALLDIR)/

doc: $(lib).cmi
	$(OCAMLDOC) -I . -html -colorize-code -d $(HTMLDOCDIR) $(lib).mli

clean:
	$(RM) -f $(sources:.ml=.cmi) $(sources:.ml=.cmo) $(sources:.ml=.cmx) \
	$(sources:.ml=.o) $(sources:.ml=.annot) $(exec) $(exec).opt

clean-all: clean
	$(RM) -f $(HTMLDOCDIR)/*.html

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

.PHONY : default test install doc clean clean-all depend distrib

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
