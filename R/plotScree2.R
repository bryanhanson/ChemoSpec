plotScree2 <-
function(pca, ...) {

# Function to make an alternate style of scree plot
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, May 2012
# pca must be of class prcomp

# Based upon idea of jrcuesta on his blog NIR-Quimiometria
	
	if (missing(pca)) stop("No PCA results provided")
	if (!("princomp" %in% class(pca) || "prcomp" %in% class(pca))) stop("Your pca results look corrupt!")
	
	eigensum <- sum(pca$sdev*pca$sdev)
	variance <- 100*(pca$sdev*pca$sdev/eigensum)
	cumvariance <- variance  # temporary definition as a vector of proper length
	for (n in c(1:length(variance))) {cumvariance[n] <- sum(variance[1:n])}

	ncp <- length(variance)
	if (ncp > 10) ncp <- 10

	plot(rep(1:ncp, each = nrow(pca$x)), as.vector(pca$x[,1:ncp]), type = "p",
		col = "red", xlab = "component", ylab = "scores",
		xlim = c(1, ncp+0.5), cex = 0.5, xaxt = "n", ...)
	axis(1, at = c(1:ncp), labels = TRUE)
	
	# label with cumulative variance
	lab.txt <- paste(round(cumvariance[1:ncp], 0), "%", sep = "")
	y.pos <- apply(pca$x[,1:ncp], MARGIN = 2, FUN = range)
	y.pos <- y.pos[2,]
	y.max <- max(y.pos)
	off <- 0.1 * y.max
	text(c(1:ncp) + 0.35, off, labels = lab.txt, cex = 0.75)
	abline(h = 0, lty = "dashed", col = "gray")

	legend("bottomright", y = NULL, pca$method, bty = "n", cex = 0.75)
	legend("topright", y = NULL, "cumulative percent variance shown to right of PC", bty = "n", cex = 0.75)
	
	}

