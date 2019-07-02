all: language-reference

language-reference: language-reference.pdc language-reference.bib
	pandoc --pdf-engine=xelatex --filter pandoc-citeproc --bibliography=language-reference.bib --csl=de-buck.csl --mathjax --toc --number-sections -o language-reference.pdf language-reference.pdc

.PHONY: all language-reference
