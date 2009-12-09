colLeaf <-
function(n, spectra) { # this is called iteratively by dendrapply

# A little trick to color leaves properly, derived from the archives
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, June 2008

	if(is.leaf(n)) {
		a <- attributes(n)
		i <- match(a$label, spectra$names)
		
		attr(n, "nodePar") <- c(a$nodePar, list(lab.col = spectra$colors[i],
			pch = NA))
		}
		n
	}

