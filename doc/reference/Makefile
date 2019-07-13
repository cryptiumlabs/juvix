all: language-reference

language-reference: src/*
	pandoc --pdf-engine=xelatex --template eisvogel --filter pandoc-include --filter pandoc-citeproc --bibliography=src/language-reference.bib --csl=de-buck.csl --mathjax --toc --number-sections -o language-reference.pdf src/language-reference.pdc

.PHONY: all language-reference
