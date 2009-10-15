q2rPCA <-
function(x) {
	
# Converts objects of class prcomp (Q-mode PCA) to class princomp (R-mode PCA)
# Bryan Hanson, DePauw Univ, Sept 2009
# original is modified by adding list elements (these could be removed to save space)

	if (!"prcomp" %in% class(x)) stop("The PCA object was not of class prcomp")

	# sdev, center and scale for both classes are the same; no change necessary
	
	x$loadings <- x$rotation
	x$scores <- x$x
	x$call <- "No call available (data converted from class prcomp)"
	x$n.obs <- dim(x$x)[1]	
	class(x) <- c("conPCA", class(x))
	x
	}

