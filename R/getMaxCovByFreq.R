

getMaxCovByFreq <- function(spectra, V = NULL, Quan = NULL) {
	
	# Function to select rows with high covariance
	# Part of the ChemoSpec package
	# Bryan Hanson, DePauw University, January 2015
	
	abs_cov = NULL # needed for checking / non-standard evaluation
	
	if (is.null(V)) stop("You must supply a covariance matrix")

	cv <- rep(NA_real_, nrow(V))
	for (i in 1:nrow(V)) {
		cvmin <- min(V[i,])  # min with sign
		cvmax <- max(V[i,])  # max with sign
		if (abs(cvmax) >= abs(cvmin)) cv[i] <- cvmax # save the most extreme value
		if (abs(cvmin) > abs(cvmax)) cv[i] <- cvmin
		}
	cva <- abs(cv)	
	
	df <- data.frame(freq = spectra$freq, cov = cv, abs_cov = cva)
	df <- arrange(df, desc(abs_cov))
	df$rel_abs_cov <- df$abs_cov/df$abs_cov[1]
	
	if (!is.null(Quan)) {
		if (!length(Quan) == 1) stop("Quan should be a single number")
		Quan <- 1 - Quan
		Q <- quantile(abs_cov, Quan)
		df <- df[, abs_cov >= Q]
		}
	
	return(df)
	}
