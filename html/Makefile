ENTRIES=$(wildcard ../entries/*.md)
.PHONY: all clean

all: blog.html

blog.html: $(ENTRIES)
	mkdir -p $(CURDIR)/../generated_figures
	rm -f generated_figures
	ln -s $(CURDIR)/../generated_figures generated_figures
	rm -f figures
	ln -s $(CURDIR)/../entries/figures figures
	nix-shell --run "pandoc $< -o $@ --from markdown --standalone --listings --filter=pandoc-citeproc --lua-filter=../pandoc_filters/tikz.lua --biblio=../bibliography/bibliography.bib --mathjax --css blog.css" ../shell.nix

clean:
	rm -f blog.html generated_figures/*.svg figures generated_figures
