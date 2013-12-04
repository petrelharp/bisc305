lectures = $(patsubst bisc305-lecture%.tex,lecture%,$(wildcard bisc305-lecture*.tex))

.PHONY : clean

.SECONDARY : 

all : bisc305-lecture15-slides.pdf bisc305-lecture16-slides.pdf bisc305-lecture17-slides.pdf bisc305-lecture18-slides.pdf bisc305-lecture19-slides.pdf bisc305-lecture20-slides.pdf bisc305-lecture21-slides.pdf bisc305-lecture22-slides.pdf bisc305-lecture23-slides.pdf bisc305-lecture24-slides.pdf bisc305-lecture25-slides.pdf bisc305-lecture26-slides.pdf 
	pdfjoin --outfile bisc305-lecture-all.pdf bisc305-lecture*-slides.pdf

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
