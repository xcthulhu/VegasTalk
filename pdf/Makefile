ENTRIES=$(wildcard ../entries/*.md)
.PHONY: all clean

all: blog.pdf

blog.pdf: $(ENTRIES)
	nix-shell --run "pandoc $< -o $@ --from markdown --template eisvogel.tex --listings --filter=pandoc-citeproc --lua-filter=tikz.lua --biblio=bibliography.bib --pdf-engine=xelatex"

clean:
	rm -rf *.pdf
