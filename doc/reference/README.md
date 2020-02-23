## Juvix Language Reference

See [the language reference](language-reference.pdf).

### Building

Built with [Pandoc](https://pandoc.org/).

Additional requirements:

- [stmaryrd](https://ctan.org/pkg/stmaryrd?lang=en), in [texlive-science](https://security.archlinux.org/package/texlive-science)
- [eisvogel LaTeX template](https://github.com/Wandmalfarbe/pandoc-latex-template)
- [pandoc-include filter](https://github.com/cwgoes/pandoc-include) (you *must* use the fork)
- [pandoc-citeproc filter](https://github.com/jgm/pandoc-citeproc)
- [xelatex](https://www.overleaf.com/learn/latex/XeLaTeX)
- [cmll.sty](https://ctan.org/pkg/cmll?lang=en), in texlive-fonts-extra for some Linux distros
- [Git LFS](https://git-lfs.github.com/) (to commit the PDF)

To update, edit source files, e.g. [src/language-reference.pdc](src/language-reference.pdc), then rebuild the document:

```bash
make language-reference
```

To automatically rebuild on file changes (`inotifywait` required):

```bash
./automake.sh
```

To spellcheck the source files (`aspell` required):

```bash
make spellcheck
```
