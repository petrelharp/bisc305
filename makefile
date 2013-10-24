.PHONY : clean checkout distclean

TEXS := bisc305-lecture13.tex bisc305-lecture14.tex bisc305-lecture15.tex
HTMLTEXS := $(patsubst %.tex,%.html,$(TEXS))
MDS := outline.md
HTMLMDS := $(patsubst %.md,%.html,$(MDS))

checkout : $(MDS) $(TEXS)
	echo "checking out of master"

clean :
	rm -f $(MDS) $(TEXS)
	-rm -f *.aux *.log *.out *.toc *.nav *.snm *.vrb texput.*

%.md : clean
	git show master:$@ > $@

%.tex : clean
	git show master:$@ > $@

%.html : %.md
	pandoc -c github-markdown.css -f markdown_github -o $@ $<


%-slides.pdf : %.tex bisc305-lecture-style.tex
	rm -f texput.*
	(cat bisc305-lecture-slides-header.tex; echo '\input{$<}') | pdflatex
	(cat bisc305-lecture-slides-header.tex; echo '\input{$<}') | pdflatex
	mv texput.pdf $@

%-print.pdf : %.tex bisc305-lecture-style.tex
	rm -f texput.*
	(cat bisc305-lecture-print-header.tex; echo '\input{$<}') | pdflatex
	(cat bisc305-lecture-print-header.tex; echo '\input{$<}') | pdflatex
	mv texput.pdf $@

