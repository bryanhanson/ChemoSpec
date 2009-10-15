r2qPCA <-
function(x) {

# Converts objects of class princomp (R-mode PCA) to class prcomp (Q-mode PCA)
# Bryan Hanson, DePauw Univ, Sept 2009
# original is modified by adding list elements (these could be removed to save space)

	if (!"princomp" %in% class(x)) stop("The PCA object was not of class princomp")
	
	# sdev, center and scale for both classes are the same; no change necessary
			
	x$rotation <- x$loadings
	x$x <- x$scores	
	class(x) <- c("conPCA", class(x))
	x
	
	}

