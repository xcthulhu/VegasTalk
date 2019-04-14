ENTRIES=$(wildcard entries/*.md)
HASKELL_ENTRIES=$(wildcard entries/*.lhs.md)
HASKELL_CHECKS=$(patsubst entries/%.lhs.md, %-haskell-check, $(HASKELL_ENTRIES))
HASKELL_BINARIES=$(patsubst entries/%.lhs.md, entries/%, $(HASKELL_ENTRIES))
.PHONY: check

all: $(HASKELL_CHECKS) blog.pdf blog.html

check: $(HASKELL_CHECKS)

blog.pdf: $(ENTRIES)
	nix-shell --run "pandoc $< -o $@ --from markdown --template eisvogel.tex --listings --filter=pandoc-citeproc --lua-filter=tikz.lua --biblio=bibliography.bib --pdf-engine=xelatex"

blog.html: $(ENTRIES)
	nix-shell --run "pandoc $< -o $@ --from markdown --standalone --listings --filter=pandoc-citeproc --lua-filter=tikz.lua --biblio=bibliography.bib --mathjax --css blog.css"

entries/%.lhs: entries/%.lhs.md
	ln -s $(CURDIR)/$< $(CURDIR)/$@

%-haskell-check: entries/%.lhs
	nix-shell --run "ghc -XRankNTypes -XExistentialQuantification -XTypeOperators -XMultiParamTypeClasses -XFunctionalDependencies -XPolyKinds -pgmL markdown-unlit $<"

clean:
	rm -rf entries/*.lhs entries/*.hi entries/*.o *.pdf $(HASKELL_BINARIES) blog.html *.svg
