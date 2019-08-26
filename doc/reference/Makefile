all: spellcheck language-reference

spellcheck:
	find ./src -type f -name "*.pdc" -exec aspell -p ./misc/aspell_dict -x -d en_GB -c {} \;

language-reference: src/*
	pandoc --pdf-engine=xelatex --template eisvogel --filter pandoc-include --filter pandoc-citeproc --bibliography=src/language-reference.bib --csl=acm-sig-proceedings.csl --mathjax --toc --number-sections -t latex -o language-reference.pdf src/language-reference.pdc

.PHONY: all spellcheck language-reference
