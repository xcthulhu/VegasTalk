ENTRIES=$(wildcard entries/*.md)
HASKELL_ENTRIES=$(wildcard entries/*.lhs.md)
HASKELL_CHECKS=$(patsubst entries/%.lhs.md, %-haskell-check, $(HASKELL_ENTRIES))
HASKELL_BINARIES=$(patsubst entries/%.lhs.md, entries/%, $(HASKELL_ENTRIES))
.PHONY: all check debug clean

all: $(HASKELL_CHECKS) documents/blog.pdf documents/blog.html

check: $(HASKELL_CHECKS)

debug: documents/blog.native

documents/blog.native: $(ENTRIES)
	mkdir -p ../generated_figures
	mkdir -p documents/
	nix-shell --run "pandoc $< -o $@ --from markdown --to native --listings --filter=pandoc-citeproc --lua-filter=pandoc_filters/tikz.lua --biblio=bibliography/bibliography.bib"

pdf/blog.pdf: $(ENTRIES)
	make -C pdf/ blog.pdf

documents/blog.pdf: pdf/blog.pdf
	mkdir -p documents/
	rm -f $(CURDIR)/documents/generated_figures
	ln -s $(CURDIR)/generated_figures $(CURDIR)/documents/generated_figures
	rm -f $(CURDIR)/documents/figures
	ln -s $(CURDIR)/entries/figures $(CURDIR)/documents/figures
	rm -f $@
	ln -s $(CURDIR)/$< $(CURDIR)/$@

html/blog.html: $(ENTRIES)
	make -C html blog.html

documents/blog.css:
	mkdir -p documents/
	rm -f $@
	ln -s $(CURDIR)/html/blog.css $(CURDIR)/$@

documents/blog.html: html/blog.html documents/blog.css
	mkdir -p documents/
	rm -f $(CURDIR)/documents/generated_figures
	ln -s $(CURDIR)/generated_figures $(CURDIR)/documents/generated_figures
	rm -f $(CURDIR)/documents/figures
	ln -s $(CURDIR)/entries/figures $(CURDIR)/documents/figures
	rm -f $@
	ln -s $(CURDIR)/$< $(CURDIR)/$@

haskell_out/%.lhs: entries/%.lhs.md
	mkdir -p haskell_out/
	ln -s $(CURDIR)/$< $(CURDIR)/$@

%-haskell-check: haskell_out/%.lhs
	nix-shell --run "ghc -XRankNTypes -XExistentialQuantification -XTypeOperators -XMultiParamTypeClasses -XFunctionalDependencies -XFlexibleInstances -XPolyKinds -XGeneralizedNewtypeDeriving -XLambdaCase -pgmL markdown-unlit $<"

clean:
	make -C pdf/ clean
	make -C html/ clean
	rm -rf haskell_out/ documents/ generated_figures/
