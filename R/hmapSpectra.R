hmapSpectra <- function(spectra, no.col = 5, cexRow = 1, cexCol = 1, ...) {
	
	# Function to display heat map of
	# seriated HCA
	# Bryan Hanson, DePauw Univ, July 2010
	# Part of the ChemoSpec package

	chkSpectra(spectra)
	
	if (no.col > 9) stop("no.col cannot be more than 9")
	x.lab <- paste(spectra$unit[1], ", reordered", sep = "")
	hmap(spectra$data, labRow = spectra$names,
		xlab = x.lab, ylab = "",
		control = list(margin = 2),
		options = list(col = brewer.pal(no.col, "Set1")),
		cexRow = cexRow, cexCol = cexCol, ...)
#	text(x = t.pos[1], y = t.pos[2], adj = t.pos[3], labels = title, cex = 1.5)
	}