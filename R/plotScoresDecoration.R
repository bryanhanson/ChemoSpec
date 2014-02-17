plotScoresDecoration <-
function(spectra, pca, pcs = c(1,2), tol = "none") {
	
# Function to do some plot annotations on the PCA score plot
# These tasks are done in all 4 cases in plotScores, so keeping it here makes
# maintaining & changing code easier.
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Aug 2009

	eigensum <- sum(pca$sdev*pca$sdev) # prepare axis labels
	variance <- 100*(pca$sdev*pca$sdev/eigensum)
	txt.x <- paste("PC", pcs[1], " score (", format(variance[pcs[1]], digits=2), "%", ")", sep = "")
	txt.y <- paste("PC", pcs[2], " score (", format(variance[pcs[2]], digits=2), "%", ")", sep = "")

	p <- pca$x[,pcs] # get info needed to label the points by sample names
	pl <- spectra$names
	
	lt <- levels(spectra$groups) # get legend stuff ready
	lc <- lt # intialize to correct length
	for (z in 1:length(lt)) {
		i <- match(lt[z], spectra$groups)
		lc[z] <- spectra$colors[i]
		}

	# now, actually label the plot
	
#	rug(pca$x[,pcs[1]])
#	rug(pca$x[,pcs[2]], side = 2)
	title(xlab = txt.x, ylab = txt.y)
	legend("topleft", y = NULL, pca$method, bty = "n", cex = 0.75)
		
	if (is.numeric(tol)) labelExtremes(p, pl, tol)
	}

