## Juvix Language Reference

See [the language reference](language-reference.pdf)

### Building

Built with [Pandoc](https://pandoc.org/).

Additional requirements:

- [eisvogel LaTeX template](https://github.com/Wandmalfarbe/pandoc-latex-template)
- [pandoc-include filter](https://pypi.org/project/pandoc-include/)
- [pandoc-citeproc filter](https://github.com/jgm/pandoc-citeproc)

To update, edit [src/language-reference.pdc](src/language-reference.pdc), then rebuild the document:

```bash
make
```
