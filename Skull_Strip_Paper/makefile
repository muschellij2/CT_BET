date=2015Jan12
stub=CT_Skull_Stripping_${date}
${stub}.pdf: ${stub}.Rnw
	Rscript -e "library(knitr); knit('${stub}.Rnw')";
	pdflatex ${stub}
	bibtex ${stub}
	pdflatex ${stub}
	pdflatex ${stub}
	open ${stub}.pdf