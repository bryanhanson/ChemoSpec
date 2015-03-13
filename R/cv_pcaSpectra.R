cv_pcaSpectra <-
function (spectra, pcs, choice = "noscale", repl = 50, segments = 4, 
    segment.type = c("random", "consecutive", "interleaved"), 
    length.seg, trace = FALSE, ...) {

# Function for CV of PCA scores (modified from Filzmosers version in {chemometrics})
# Part of the ChemoSpec package.  Bryan Hanson, DePauw Univ, Sept 2009
# Conducts classical, not robust, PCA

	if (!requireNamespace("pls", quietly = TRUE)) {
		stop("You need to install package pls to use this function")
	}

	if (missing(spectra)) stop("No spectral data set passed to PCA")
	if (!class(spectra) == "Spectra") stop("Your spectral data set looks corrupt!")
	
	choices <- c("noscale", "autoscale", "Pareto") # trap for invalid scaling method
	check <- choice %in% choices
	if (!check) stop("The choice of scaling parameter was invalid")
	
	# First, row scale (compensates for different dilutions/handling of samples)

	t.data <- t(spectra$data)
	sums <- colSums(t.data)
	row.scaled <- t(scale(t.data, center = FALSE, scale = sums))

	# Center & scale the data using the desired method.

	if (identical(choice, "noscale")) {centscaled <- scale(row.scaled, center = TRUE, scale = FALSE)}
	
	if (identical(choice, "autoscale")) {
		col.sd <- sd(row.scaled)
		centscaled <- scale(row.scaled, center = TRUE, scale = col.sd)}

	if (identical(choice, "Pareto")) {
		col.sd <- sd(row.scaled)
		centscaled <- scale(row.scaled, center = TRUE, scale = col.sd^0.5)}

	# Filzmoser's stuff follows... (modified slightly)

	X <- centscaled
	amax <- pcs
    if (missing(amax)) {
        amax <- 5
    }
    else {
        if (amax < 1 || amax > min(nrow(X) - 1, ncol(X))) 
            stop("Invalid number of components, amax")
    }
    amax <- min(amax, nrow(X) - max(sapply(segments, length)) - 
        1)
    optcomp <- matrix(NA, nrow = segments, ncol = repl)
    MSEP <- matrix(NA, nrow = repl, ncol = amax)
    dimnames(MSEP) <- list(paste("rep", 1:repl), paste("PC", 
        1:amax))
    Fit <- matrix(NA, nrow = repl, ncol = amax)
    dimnames(Fit) <- list(paste("rep", 1:repl), 1:amax)
    for (i in 1:repl) {
        if (missing(length.seg)) {
            segment <- pls::cvsegments(nrow(X), k = segments, type = segment.type)
        }
        else {
            segment <- pls::cvsegments(nrow(X), length.seg = length.seg, 
                type = segment.type)
        }
        if (trace) 
            cat(paste("Replication: ", i))
        MSEPj <- matrix(NA, nrow = segments, ncol = amax)
        Fitj <- matrix(NA, nrow = segments, ncol = amax)
        for (n.seg in 1:length(segment)) {
            if (trace) 
                cat(n.seg, "")
            seg <- segment[[n.seg]]
            obsuse <- as.numeric(unlist(segment[-n.seg]))
            Xtrain <- X[obsuse, ]
            obstest <- as.numeric(unlist(segment[n.seg]))
            Xtest <- X[obstest, ]
            if (ncol(Xtrain) > nrow(Xtrain)) {
                e <- eigen(Xtrain %*% t(Xtrain))
                Ttrain <- e$vectors %*% diag(sqrt(e$values))
                Ptrain <- t(Xtrain) %*% Ttrain %*% diag(1/e$values)
            }
            else {
                Xtrain_svd <- svd(Xtrain)
                Ptrain <- Xtrain_svd$v
            }
            Ttest <- Xtest %*% Ptrain
            for (j in 1:amax) {
                MSEPj[n.seg, j] <- sum((Xtest - Ttest[, 1:j] %*% 
                  t(Ptrain[, 1:j]))^2)
            }
            Fitj[n.seg, ] <- MSEPj[n.seg, ]/sum(Xtest^2)
        }
        MSEP[i, ] <- apply(MSEPj, 2, mean)
        Fit[i, ] <- 1 - apply(Fitj, 2, mean)
    }
    
    # The plotting details have been modified quite a bit
    
    boxplot(as.data.frame(Fit), ylab = "Explained variance", 
            xlab = "Number of components", ...)
    # construct a legend based upon values of center & scale
	note <- paste("centered/", choice, "/", "classical", sep = "")
    leg.txt <- paste(spectra$desc, note, sep = " ")
	legend("bottomright", leg.txt, bty = "n", cex = 0.75)
    
    list(ExplVar = Fit, MSEP = MSEP)
}

