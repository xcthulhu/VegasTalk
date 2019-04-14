ENTRIES=$(wildcard entries/*.md)
HASKELL_ENTRIES=$(wildcard entries/*.lhs.md)
HASKELL_CHECKS=$(patsubst entries/%.lhs.md, %-haskell-check, $(HASKELL_ENTRIES))
HASKELL_BINARIES=$(patsubst entries/%.lhs.md, entries/%, $(HASKELL_ENTRIES))
.PHONY: all check clean

all: $(HASKELL_CHECKS) documents/blog.pdf documents/blog.html

check: $(HASKELL_CHECKS)

pdf/blog.pdf: $(ENTRIES)
	make -C pdf/ blog.pdf

documents/blog.pdf: pdf/blog.pdf
	mkdir -p documents/
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
	rm -f $@
	ln -s $(CURDIR)/$< $(CURDIR)/$@

haskell_out/%.lhs: entries/%.lhs.md
	mkdir -p haskell_out/
	ln -s $(CURDIR)/$< $(CURDIR)/$@

%-haskell-check: haskell_out/%.lhs
	nix-shell --run "ghc -XRankNTypes -XExistentialQuantification -XTypeOperators -XMultiParamTypeClasses -XFunctionalDependencies -XPolyKinds -pgmL markdown-unlit $<"

clean:
	make -C pdf/ clean
	make -C html/ clean
	rm -rf haskell_out/ documents/ generated_figures/
