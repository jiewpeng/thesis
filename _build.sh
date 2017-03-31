#!/bin/sh

Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::pdf_book')"

# Acrobat.exe "_book/_main.pdf"
