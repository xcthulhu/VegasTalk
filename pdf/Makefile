ENTRIES=$(wildcard ../entries/*.md)
.PHONY: all clean

all: blog.pdf

blog.pdf: $(ENTRIES)
	mkdir -p $(CURDIR)/../generated_figures
	rm -f generated_figures
	ln -s $(CURDIR)/../generated_figures generated_figures
	rm -f figures
	ln -s $(CURDIR)/../entries/figures figures
	nix-shell --run "pandoc $< -o $@ --from markdown --template eisvogel.tex --listings --filter=pandoc-citeproc --lua-filter=../pandoc_filters/tikz.lua --biblio=../bibliography/bibliography.bib --pdf-engine=xelatex" ../shell.nix

clean:
	rm -f blog.pdf generated_figures/*.pdf figures generated_figures
