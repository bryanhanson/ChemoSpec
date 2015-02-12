

getMaxCovByFreq <- function(spectra, V = NULL, Quan = NULL) {
	
	# Function to select rows with high covariance
	# Part of the ChemoSpec package
	# Bryan Hanson, DePauw University, January 2015
	
	if (is.null(V)) stop("You must supply a covariance matrix")
	abs_cov <- NA
	cv <- rep(NA_real_, nrow(V))
	for (i in 1:nrow(V)) cv[i] <- abs(max(V[i,]))
	df <- data.frame(freq = spectra$freq, abs_cov = cv)
	df <- arrange(df, desc(abs_cov))
	df$rel_cov <- df$abs_cov/df$abs_cov[1]
	if (!is.null(Quan)) {
		if (!length(Quan) == 1) stop("Quan should be a single number")
		Quan <- 1 - Quan
		Q <- quantile(cv, Quan)
		df <- subset(df, abs_cov >= Q)
		}
	
	return(df)
	}
