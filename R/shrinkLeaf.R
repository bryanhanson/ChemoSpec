shrinkLeaf <-
function(n, spectra) { # this is called iteratively by dendrapply

# A little trick to color leaves properly, derived from the archives
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, June 2008

	lab.size = 1.0
	if(length(spectra$names) > 20) lab.size = 0.75
	if(length(spectra$names) > 50) lab.size = 0.5
	
	if(is.leaf(n)) {
		a <- attributes(n)
		i <- match(a$label, spectra$names)

		attr(n, "nodePar") <- c(a$nodePar, list(lab.col = "black",
			pch = NA, lab.cex = lab.size))
		}
		n
	}

