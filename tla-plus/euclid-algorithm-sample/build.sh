#!/bin/bash

tla2tex Euclid.tla
pdflatex Euclid.tex

java -cp .:/usr/share/java/tla-tools tla2sany.SANY Euclid.tla
java -cp .:/usr/share/java/tla-tools tlc2.TLC  Euclid.tla