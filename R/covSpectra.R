

covSpectra <- function(spectra, freq = spectra$freq[1],
	C = NULL, V = NULL, yFree = TRUE, ...) {
	
# Function to carry out Nicholson's STOCSY analysis
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, January 2015

# For large data sets, there are extreme challenges with cor()

# NOTE: Cannot subset before computing cor() as this gives the wrong numerical answer

	if (missing(spectra)) stop("No spectral data provided")
	chkSpectra(spectra)

	# Check to see if spectra$freq is increasing - if not, findInterval will fail
	# Silently reverse things
	if (is.unsorted(spectra$freq)) {
		spectra$freq <- rev(spectra$freq)
		spectra$data <- spectra$data[,ncol(spectra$data):1]
		}
		
	row <- findInterval(freq, spectra$freq)
	
	if (is.null(C)) { # user did not provide pre-computed correlation matrix
		X <- spectra$data
		if (ncol(X) > 10000) message("Calculating cor() may take a few minutes")
		C <- cor(X) # same as (t(X) %*% X)/(nrow(spectra$data) - 1)
		}
	
	if (is.null(V)) { # user did not provide pre-computed covariance matrix
		X <- spectra$data
		if (ncol(X) > 10000) message("Calculating cov() may take a few minutes")
		V <- cov(X) # same as (t(X) %*% X)/(nrow(spectra$data) - 1)
		}

	# Color scale for each level
	# blue/low -> red/high, anchored at zero (index 5, a shade of green)
	# max and min will come from the data (i.e., red will be at max of V)
	cscale <- c(rev(rainbow(4, start = 0.45, end = 0.66)), rev(rainbow(5, start = 0.0, end = 0.25)))
	# view with:
	# pie(rep(1, 9), col = cscale)

	refscale <- seq(-1, 1, length.out = 9)
	
	# Now average every contiguous pair of values in C[row,] so that there is one
	# less value, and use the mean value of the pair to assign colors
	# e.g. the mean of points n & n+1 determines the color used to plot that segment

	cd <- diff(C[row,])
	cr <- 0.5 * cd + C[row,][-length(C[row,])] # this will have one value less than the data
	myc <- cscale[findInterval(cr, refscale)] # color based upon cor, not cov

	# Ready to plot
	
	np <- length(spectra$freq)
	ind1 <- 1:(np-1)
	ind2 <- 2:np
	
	if (yFree) {
		plot(spectra$freq, V[row,], type = "n",
			xlab = spectra$unit[1], ylab = "covariance",
			main = paste("Frequency = ", sprintf("%5.5f", spectra$freq[row]), sep = ""),
			...)
		segments(spectra$freq[ind1], V[row, ind1], spectra$freq[ind2], V[row, ind2],  col = myc, ...)
		abline(v = spectra$freq[row], lty = 2, col = "gray")
		}
		
	if (!yFree) {
		plot(spectra$freq, V[row,], type = "n", ylim = range(V),
			xlab = spectra$unit[1], ylab = "covariance",
			main = paste("Frequency = ", sprintf("%5.5f", spectra$freq[row]), sep = ""),
			...)
		segments(spectra$freq[ind1], V[row, ind1], spectra$freq[ind2], V[row, ind2],  col = myc, ...)
		abline(v = spectra$freq[row], lty = 2, col = "gray")
		}
		
	L <- list(cov = V, cor = C)
	invisible(L)
	
	}
		
