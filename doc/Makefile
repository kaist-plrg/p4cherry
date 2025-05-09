# Configuration

NAME = watsup
EXE = $(PWD)/../../$(NAME)
EXT = $(NAME)
LOG = _log

OWNDIR = $(PWD)
SPECDIR = ../spec


# Main targets

.PHONY: all

all: pdf html

# Sphinx splicing

SPHINXBUILD = sphinx-build
SPHINXDIR = _sphinx
BUILDDIR = _sphinx/_build

CONF = doc/conf.py
MATHDEF = doc/util/mathdef.py
UTIL = util

INPUTFILES = $(wildcard $(SPHINXDIR)/*/*.rst)
SPLICEINPUT = $(strip $(patsubst $(SPHINXDIR)/%,%,$(subst " ", " ", $(INPUTFILES))))

.PHONY: document

document: doc $(CONF) $(MATHDEF)

.PHONY: setup

setup: document
	mkdir -p $(BUILDDIR)
	cp -r doc/* $(SPHINXDIR)

.PHONY: splice

splice: setup 
	cd $(SPHINXDIR) && $(EXE) -l ../$(SPECDIR)/*.$(EXT) --splice-sphinx -p $(SPLICEINPUT) -w -i
	cd ..

.PHONY: html

html: splice
	$(SPHINXBUILD) -b html -d $(SPHINXDIR)/doctrees $(SPHINXDIR) $(BUILDDIR)/html
	@echo "html finished; the HTML pages are in $(BUILDDIR)/html."

.PHONY: pdf

pdf: latexpdf 
	cp $(BUILDDIR)/latex/*.pdf .
	@echo "pdf finished; the PDF files are in the current directory."

.PHONY: latexpdf

latexpdf: splice
	$(SPHINXBUILD) -b latex -d $(SPHINXDIR)/doctrees $(SPHINXDIR) $(BUILDDIR)/latex
	$(MAKE) -C $(BUILDDIR)/latex LATEXMKOPTS=" </dev/null" all-pdf >$(BUILDDIR)/latex/LOG 2>&1 || cat $(BUILDDIR)/latex/LOG
	@echo "pdflatex finished; the PDF files are in $(BUILDDIR)/latex."


# Cleanup

.PHONY: clean distclean

clean:
	dune clean
	rm -rf $(SPHINXDIR) $(LOG)
	rm -f *.pdf

distclean: clean
