slides = $(patsubst bisc305-lecture%.tex,bisc305-lecture%-slides.pdf,$(wildcard bisc305-lecture*.tex))
prints = $(patsubst bisc305-lecture%.tex,bisc305-lecture%-print.pdf,$(wildcard bisc305-lecture*.tex))

.PHONY : clean

.SECONDARY : 

bisc305-lecture-all.pdf : $(slides)
	pdfjoin --outfile bisc305-lecture-all.pdf bisc305-lecture*-slides.pdf

lecture.% : bisc305-lecture%-slides.pdf bisc305-lecture%-print.pdf
	echo "making " $@

%-slides.pdf : %.tex resources/bisc305-lecture-style.tex
	rm -f texput.*
	(cat resources/bisc305-lecture-slides-header.tex; echo '\input{$<}') | pdflatex
	(cat resources/bisc305-lecture-slides-header.tex; echo '\input{$<}') | pdflatex
	mv texput.pdf $@

%-print.pdf : %.tex resources/bisc305-lecture-style.tex
	rm -f texput.*
	(cat resources/bisc305-lecture-print-header.tex; echo '\input{$<}') | pdflatex
	(cat resources/bisc305-lecture-print-header.tex; echo '\input{$<}') | pdflatex
	mv texput.pdf $@

clean : 
	-rm -f *.aux *.log *.out *.toc *.nav *.snm *.vrb texput.*

%.html : %.md
	pandoc -c github-markdown.css -f markdown_github -o $@ $<
