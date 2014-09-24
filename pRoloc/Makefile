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

all: HUPO_2014_poster.pdf

setvars:
ifeq (${R_HOME},)
R_HOME= $(shell R RHOME)
endif


HUPO_2014_poster.pdf: HUPO_2014_poster.tex
	pdflatex HUPO_2014_poster.tex

HUPO_2014_poster.tex: HUPO_2014_poster.Rnw
	"$(R_HOME)/bin/R" --vanilla -e "knitr::knit2pdf('HUPO_2014_poster.Rnw')"


.PHONY: all clean

clean:
	rm -f *~
	rm -f $(LATEXFILES)
	rm -f .Rhistory .Makefile.swp
