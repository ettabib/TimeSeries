.PHONY: all, update

CC = R CMD Sweave 
CFLAGS =  
LDFLAGS = 
EXEC =  
SRC=$(wildcard *.rnw)
OBJ = $(SRC:.rnw=.tex) 

all: update 

debug:
	@echo "OBJ=$(OBJ)"

%.tex:%.rnw
	@cecho -yellow  "***** Reconverting Sweave ----> Latex *****" 
	@-echo "Sweave ('$<', encoding='latin1')" | R --slave
update:clean rapport.tex $(OBJ)
	@-pdflatex -interaction=nonstopmode rapport.tex > /dev/null 2> /dev/null
	@-cecho -yellow "-------< First compilation done >-------"
	@echo " "
	@-makeindex rapport.idx
	@cecho -yellow "-------< Index compilation done >-------"
	@-pdflatex -interaction=nonstopmode rapport.tex > /dev/null 2> /dev/null
	@-cecho -yellow "-------< Third compilation done >-------"
	@-echo " "
	@-bibtex biblio.bib > /dev/null 2> /dev/null
	@-cecho -green "-------< Compiling bibtex done >-------"	
	@-echo " "
	@-pdflatex -interaction=nonstopmode rapport.tex > /dev/null 2> /dev/null
	@-cecho -yellow "-------< Second compilation done >-------"
	@-echo " "
	@date | awk '{print $5}'


clean:
	@cecho -yellow  "***** Cleaning Latex's Sweave files *****"
	@-rm -rf $(OBJ)

view:rapport.pdf
	@open rapport.pdf

mrproper: clean
	@-rm -f rapport.pdf > /dev/null 2> /dev/null
	@-rm -rf *.bbl *.idx *.blg *.toc *.log *.rel *.fmt *.prv *.aux > /dev/null 2> /dev/null
