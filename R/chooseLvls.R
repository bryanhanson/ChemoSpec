
chooseLvls <- function(M, n = 10, mode = "even",
	lambda = 1.0, base = 2,
	showHist = FALSE, ...) {

# Function to choose levels for contour, image plots
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, December 2014

	# Helper function for showHist = TRUE
	
	sH <- function(M, lvs, ...) {
		
		# Check which arm is larger and use that for xlim
		ref <- findExtreme(M)
		lim.x <- c(-ref, ref)

		def.par <- par(no.readonly = TRUE)
		nf <- layout(mat = matrix(c(1, 2), 2, 1, byrow = TRUE), heights = c(6 , 1))
		par(mar = c(3.1, 3.1, 1.1, 2.1))

		hist(M, breaks = 50, main = "",
			xlab = "", ylab = "", xlim = lim.x,
			col = "black", ...)
		abline(v = lvs, col = "pink", lty = 2)
		
		col1 <- rev(rainbow(5, start = 0.0, end = 0.25))
		col2 <- rev(rainbow(4, start = 0.45, end = 0.66))
		cscale <- c(col2, col1)
		nc <- length(cscale)
		plot(1:nc, rep(1.0, nc), type = "n",
			yaxt = "n", xaxt = "n", main = "", xlab = "", ylab = "")
		for (i in 1:nc) {
			rect(i-0.5, 0.5, i+0.5, 1.5, border = NA, col = cscale[i])
			}
			
		par(def.par)
		} # end of sH

	# More helper functions
	
	findExtreme <- function(M) {
		# Find the most extreme value in a numerical object (matrix)
		# and return the absolute value of that extreme
		
		ex <- abs(range(M))
		ex <- ex[which.max(ex)]
		return(ex)
		}
		
	getPN <- function(M) {
		# Get either the (+)-ive or (-)-ive values in a matrix
		# depending upon which is most extreme in absolute terms,
		# and return them as a vector of positive values
		
		neg <- M[M <= 0] # these are vectors
		pos <- M[M > 0]
		if (length(pos) == 0) return(neg) # no + values
		if (length(neg) == 0) return(pos) # no - values
		exP <- findExtreme(pos)		
		exN <- findExtreme(neg)
		if (exP >= exN) return(pos)
		if (exN > exP) return(abs(neg))
		}
			
			
	if (mode == "even") {
		n <- as.integer(n)
		mn <- min(M)
		mx <- max(M)
		lower <- mn * 0.95 # shrink in a bit at both ends
		upper <- mx * 0.95
		lvs <- seq(lower, upper, length.out = n)
		if (showHist) sH(M, lvs, ...)
		return(lvs)
		}


	if (mode == "exp") { # For use when the range is [-Inf...Inf]
		n <- as.integer(n)
		
		if (lambda == 0.0) stop("lambda cannot be zero")
		
		# Compute levels (only working with positive values)
				
		ref <- findExtreme(M)
		lower <- 0.001 # just above zero to avoid Inf
		lvs <- seq(log(lower), log(ref), length.out = floor(n/2))
		lvs <- exp(lvs*lambda)
		
		# Now scale back into the range of the data
		# Shrink inward a bit, can't show a contour at max/min value
		lvs2 <- lvs * ref*0.975/max(lvs)

		# Reflect through zero
		lvs <- sort(c(-1*lvs2, lvs2))
		if (showHist) sH(M, lvs, ...)
		return(lvs)
		}


	if (mode == "log") { # For use when the range is [-Inf...Inf]
		n <- as.integer(n)
		if (base <= 0L) stop("base must be > 0")
		# Compute levels (only working with positive values)

		ref <- findExtreme(M)				
		X <- getPN(M)
		lower <- 0.001 # just above zero to avoid Inf
		lvs <- seq(lower, ref, length.out = floor(n/2))
		lvs <- log(lvs, base)
		# Now scale back into the range of the data
		lvs <- abs(min(lvs)) + lvs
		sf <- diff(range(X))/diff(range(lvs))
		lvs <- lvs * sf
		lvs[1] <- 0.1 * ref # a little fudge to stay off zero & near the mirror value
		
		# Reflect through zero
		lvs <- sort(c(-1*lvs, lvs))
		if (showHist) sH(M, lvs, ...)
		return(lvs)
		}


	if (mode == "ecdf") {
		if ((any(n > 1)) | (any(n < 0))) stop("For ecdf mode, n must be a vector of values on [0...1]")
		Fn <- ecdf(sort(M))
		P <- Fn(sort(M))
		lv <- c()
		for (i in 1:length(n)) {
			tmp <- max(grep(n[i], P))
			lv <- c(lv, tmp)	
			}
		lvs <- sort(M)[lv]
		if (showHist) sH(M, lvs, ...)
		return(lvs)
		}

	if (mode == "posexp") { # For use when you just want (+)-ive
		lvs <- chooseLvls(M = M, n = n, mode = "exp", lambda = lambda, base = base, ...)
		lvs <- lvs[lvs > 0]
		if (showHist) sH(M, lvs, ...)
		return(lvs)
		}


	if (mode == "negexp") { # For use when you just want (-)-ive
		lvs <- chooseLvls(M = M, n = n, mode = "exp", lambda = lambda, base = base, ...)
		lvs <- lvs[lvs < 0]
		if (showHist) sH(M, lvs, ...)
		return(lvs)
		}

	if (mode == "poslog") { # For use when you just want (+)-ive
		lvs <- chooseLvls(M = M, n = n, mode = "log", lambda = lambda, base = base, ...)
		lvs <- lvs[lvs > 0]
		if (showHist) sH(M, lvs, ...)
		return(lvs)
		}


	if (mode == "neglog") { # For use when you just want (-)-ive
		lvs <- chooseLvls(M = M, n = n, mode = "log", lambda = lambda, base = base, ...)
		lvs <- lvs[lvs < 0]
		if (showHist) sH(M, lvs, ...)
		return(lvs)
		}

	}
