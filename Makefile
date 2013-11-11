all:
	pdfcrop figs_lattice.pdf
	xelatex vizRguide
	bibtex vizRguide
	makeindex vizRguide
	xelatex vizRguide
	xelatex vizRguide
