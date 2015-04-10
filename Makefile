all:
	pdfcrop figs_lattice.pdf
	xelatex vizRguide
	biber vizRguide
	makeindex vizRguide
	xelatex vizRguide
	xelatex vizRguide
