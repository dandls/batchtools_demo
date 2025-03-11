all : 
	Rscript -e "knitr::knit('slides.Rnw')"
	texi2pdf slides.tex

clean: 
	rm slides.nav
	rm slides.out
	rm slides.aux
	rm slides.snm
	rm slides.toc
	rm slides.tex
	rm -r knitr/
