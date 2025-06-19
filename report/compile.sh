#!/bin/bash

filename='report'

pdflatex $filename.tex
pdflatex $filename.tex

rm $filename.toc $filename.aux $filename.log $filename.out
