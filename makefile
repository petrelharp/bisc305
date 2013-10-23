lectures = $(patsubst bisc305-lecture%.tex,lecture%,$(wildcard bisc305-lecture*.tex))

.PHONY : clean

.SECONDARY : 

lecture.% : bisc305-lecture%-slides.pdf bisc305-lecture%-print.pdf
	echo "making " $@

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

clean : 
	-rm -f *.aux *.log *.out *.toc *.nav *.snm *.vrb texput.*

%.html : %.md
	pandoc -c github-markdown.css -f markdown_github -o $@ $<
