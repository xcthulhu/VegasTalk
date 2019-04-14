ENTRIES=$(wildcard ../entries/*.md)
.PHONY: all clean

all: blog.html

blog.html: $(ENTRIES)
	mkdir -p ../generated_figures
	rm -f generated_figures
	ln -s ../generated_figures .
	nix-shell --run "pandoc $< -o $@ --from markdown --standalone --listings --filter=pandoc-citeproc --lua-filter=../pandoc_filters/tikz.lua --biblio=../bibliography/bibliography.bib --mathjax --css blog.css" ../shell.nix

clean:
	rm -rf blog.html generated_figures/*.svg generated_figures
