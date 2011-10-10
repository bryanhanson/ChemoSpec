binData <- function(x = NULL, y = NULL, bin.ratio = 2) {
	
# Function to bin or bucket spectral data
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, November 2009

	# Be careful:
	# bin.ratio may not divide evenly into no. data points
	# Drop a few data points on one end so that it does

	if (bin.ratio <= 1) stop("bin.ratio must > 1")
	if (!isWholeNo(bin.ratio)) stop("bin.ratio must be an integer > 1")
	if (!is.null(y) && !is.null(x)) {
		if (!identical(length(x), length(y))) stop("x and y vectors in binData have different lengths")
		}
	chk <- check4Gaps(x, silent = TRUE)
	if (length(chk) > 1) stop("The data being binned has gaps and cannot be binned accurately")
	br <- bin.ratio
	
	if (!is.null(x)) len <- length(x)
	if (!is.null(y)) len <- length(y)
	no.bins <- len/br # initial value; maybe final too
	if (!isWholeNo(no.bins)) { # trim data just a bit so no.bins is a whole number
		chop <- NULL
		n <- 0
		while (n < br) {
			n <- n + 1
			l <- len - n
			no.b <- l/br
			if (isWholeNo(no.b)) { chop <- n; break }
			}
		rem <- c(1:chop) # chop off the first few data points
		# warning(chop, " data points were removed from the start of the data to make it divisible by bin.ratio") # no need to warn here, 
		if (!is.null(x)) x <- x[-rem]
		if (!is.null(y)) y <- y[-rem]
		if (!is.null(x)) len <- length(x)
		if (!is.null(y)) len <- length(y)
		no.bins <- len/br # reset these values
		}

	# Using data grouped in sets of bin.ratio length, avg the x values and sum the y-values
	# Three cases possible: only x is provided, only y is provided, both are provided

	b.x <- c(rep(NA, no.bins))
	b.y <- c(rep(NA, no.bins))
	
	cnt <- seq(1, len, br) # length of cnt will = no.bins
	inc <- br - 1
	
	if (!is.null(x)) {
		for (n in 1:no.bins) {
			r <- c(cnt[n]:(cnt[n] + inc))
			b.x[n] <- mean(x[r])
			}
		}
		
	if (!is.null(y)) {
		for (n in 1:no.bins) {
			r <- c(cnt[n]:(cnt[n] + inc))
			b.y[n] <- sum(y[r])
			}
		}
	
	if (!is.null(y)) res <- data.frame(sum.y = b.y)
	if (!is.null(x)) res <- data.frame(mean.x = b.x)
	if (!is.null(y) && !is.null(x)) res <- data.frame(mean.x = b.x, sum.y = b.y)
	res
	}
