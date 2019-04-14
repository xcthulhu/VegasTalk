ENTRIES=$(wildcard ../entries/*.md)
.PHONY: all clean

all: blog.pdf

blog.pdf: $(ENTRIES)
	mkdir -p ../generated_figures
	rm -f generated_figures
	ln -s ../generated_figures .
	nix-shell --run "pandoc $< -o $@ --from markdown --template eisvogel.tex --listings --filter=pandoc-citeproc --lua-filter=../pandoc_filters/tikz.lua --biblio=../bibliography/bibliography.bib --pdf-engine=xelatex" ../shell.nix

clean:
	rm -rf *.pdf generated_figures/*.pdf generated_figures
