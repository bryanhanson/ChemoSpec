

sortCrossPeaks <- function(spectra, V = NULL, window = NULL, Quan = NULL,
	byFreq = FALSE, freqThres = 0.01, ...) {
	
	# Function to select rows with high covariance, ignoring the region near the diagonal
	# Part of the ChemoSpec package
	# Bryan Hanson, DePauw University, January 2015
	
	message("Beta version of sortCrossPeaks, check your results carefully")
	
	abs_max_cov = NULL # needed for checking / non-standard evaluation
	freq = NULL # needed for checking / non-standard evaluation
	
	if (is.null(V)) V <- cov(spectra$data)
	
	# Remove entries in V near the diagonal (diagonal is variance)
	# window as passed is a percentage of the rows. After conversion, it is the number of
	# entries on either side of the diagonal to remove.
	
	if (!is.null(window)) {
		if ((window < 0) | (window > 1)) stop("The value of window must be in (0..1)")
		window <- ceiling(window*nrow(V))
		}
	if (is.null(window)) window <- ceiling(0.1*nrow(V))
		
	if (window <= 1) {
		window <- 0
		message("Only removing the diagonal, you may wish to increase window")
		}
	
	# Next trick inspired by the code for upper.tri
	wh <- col(V) > row(V) + window # assumes square matrix! (which we have)
	V[!wh] <- NA
	
	# And this one is based upon http://stackoverflow.com/a/29857455/633251
	
	V <- pmax(V, t(V), na.rm = TRUE) # Now it is symmetric
	
	# Now locate and organize by the extreme values
	
	cv <- rep(NA_real_, nrow(V))
	for (i in 1:nrow(V)) {
		cvmin <- min(V[i,], na.rm = TRUE)  # min with sign
		cvmax <- max(V[i,], na.rm = TRUE)  # max with sign
		if (abs(cvmax) >= abs(cvmin)) cv[i] <- cvmax # save the most extreme value
		if (abs(cvmin) > abs(cvmax)) cv[i] <- cvmin
		}
	cva <- abs(cv)	
	
	df <- data.frame(freq = spectra$freq, max_cov = cv, abs_max_cov = cva)
	df <- arrange(df, desc(abs_max_cov))
	df$rel_abs_max_cov <- df$abs_max_cov/df$abs_max_cov[1]
		
	# Return only a portion if requested
	
	if (!is.null(Quan)) {
		if (!length(Quan) == 1) stop("Quan should be a single number")
		Quan <- 1 - Quan
		Q <- quantile(cva, Quan)
		keep <- which(df$abs_max_cov >= Q)
		df <- df[keep, ]

	# byFreq = TRUE sort instead by freq, then collapse rows
	
		if (byFreq) {
			df <- arrange(df, freq)
			df <- collapseRowsOrCols(as.matrix(df), 1, thres = freqThres,...)
			} # end of byFreq = TRUE
		
	} # end of !is.null(Quan)
	
	return(as.data.frame(df))
	}
