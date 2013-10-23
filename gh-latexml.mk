.PHONY : clean

TEXS := bisc305-lecture13.tex bisc305-lecture14.tex
HTMLTEXS := $(patsubst %.tex,%.html,$(TEXS))
MDS := outline.md
HTMLMDS := $(patsubst %.md,%.html,$(MDS))



%.html : %.md
	pandoc -c github-markdown.css -f markdown_github -o $@ $<

