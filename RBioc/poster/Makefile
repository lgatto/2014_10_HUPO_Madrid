LATEXFILES = *.aux\
	*.snm\
	*.nav\
	*.vrb\
	*.bbl\
	*.blg\
	*.ilg\
	*.log\
	*.nlo\
	*.nls\
	*.toc\
	*.aux\
	Rplots.pdf\
	*.dvi\
	*.map\
	*.out\
	*.figlist\
	*.dep\
	*.dpth\
	*-blx.bib\
	*.run.xml\
	*.makefile

all: poster 

.PHONY: clean poster

ifeq (${R_HOME},)
R_HOME= $(shell R RHOME)
endif

HUPO2014_rbioc_poster.pdf: HUPO2014_rbioc_poster.Rnw
	"$(R_HOME)/bin/R" --vanilla -e "knitr::knit2pdf('HUPO2014_rbioc_poster.Rnw')"
	pdflatex HUPO2014_rbioc_poster.tex
	pdflatex HUPO2014_rbioc_poster.tex

poster: HUPO2014_rbioc_poster.pdf

clean:
	rm -f $(LATEXFILES)
	rm -f .Rhistory
	rm -f HUPO2014_rbioc_poster.tex
	rm -f *~
	rm -f */*~
	rm -f output*xml
