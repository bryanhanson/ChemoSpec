plotScree <-
function(pca,  ...) {

# Function to do the scree plot
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, June 2008
# pca must be of class prcomp
	
	if (missing(pca)) stop("No PCA results provided")
	if (!("princomp" %in% class(pca) || "prcomp" %in% class(pca))) stop("Your pca results look corrupt!")
	
	eigensum <- sum(pca$sdev*pca$sdev)
	variance <- 100*(pca$sdev*pca$sdev/eigensum)
	cumvariance <- variance  # temporary definition as a vector of proper length
	for (n in c(1:length(variance))) {cumvariance[n] <- sum(variance[1:n])}

	ncp <- length(variance)
	if (ncp > 10) ncp <- 10
	
	plot(c(1:ncp), variance[1:ncp], type = "l", col = "red", xlab = "factor", ylab = "percent", ylim = c(0,100), ...)
	axis(1, at = c(1:ncp), labels = TRUE)
	points(c(1:ncp), cumvariance[1:ncp], type="l", col="blue")
	
	abline(v = c(1:ncp), h = c(0,10,20,30,40,50,60,70,80,90,100), col = "lightgray")
	abline(h = 95, lty = "dashed")
	legend("bottomleft", y = NULL, pca$method, bty = "n", cex = 0.75)
	legend("topright", y=NULL, "cumulative percent", lty = 1, bty = "n", inset = c(0, 0.40), col = "blue", cex = 0.75)
	legend("topright",y = NULL, " individual percent", lty = 1, bty = "n", inset = c(0, 0.50), col = "red", cex = 0.75)
	
	}

