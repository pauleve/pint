# Minimal makefile for Sphinx documentation
#

# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
SPHINXPROJ    = Pint
SOURCEDIR     = src
BUILDDIR      = build
OCAMLBUILDDIR      = $(BUILDDIR)/ocaml-api

# Put it first so that "make" without argument is like "make help".
help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: help Makefile

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
%: Makefile
	@$(SPHINXBUILD) -M $(@) "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

ocamlapi:
	rm -rf $(OCAMLBUILDDIR)/*
	mkdir -p $(OCAMLBUILDDIR)
	ocamldoc -sort -html -d $(OCAMLBUILDDIR) -I ../bindings -I ../pintlib -I ../anlib -I ../phlib \
		-t "Pint OCaml API" \
		../bindings/r.mli ../pintlib/*.mli ../anlib/*.mli ../phlib/*.mli ../*.mli

depclean:
	rm -rf $(BUILDDIR)/*
clean:
	rm -rf $(BUILDDIR)/html

