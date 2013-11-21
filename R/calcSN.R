calcSN <- function (x, span, idx, debug = FALSE) {
	
	# Part of ChemoSpec, December 2012, Bryan A. Hanson

	# Given a peak found at index idx in vector x, using span,
	# calculate a simple measure of S/N
	# No baseline correction done here
	# idx is not vectorized!!!

	# Set up window indices to define the main peak
	# and two regions of 'noise' on either side
	
	ind4 <- c(
		idx - floor(0.5*span) - span, # far left end
		idx - floor(0.5*span), # left edge of peak
		idx + floor(0.5*span), # right edge of peak
		idx + floor(0.5*span) + span # far right end
		)
	
	# Check for peaks near the ends (which might put ind4 out of bounds)

	pR <- FALSE
	pL <- FALSE		
	
	if (any(ind4 < 1)) {
		if (ind4[1] < 1) ind4[1] <- 1
		if (ind4[2] < 1) ind4[2] <- 1
		if (ind4[3] < 1) ind4[3] <- 1
		if (ind4[4] < 1) ind4[4] <- 1
		cat("\n>>> Peak near the left end, see ?calcSN & inspect the results carefully\n")
		pL <- TRUE # flag for peak near left edge
		}

	if (any(ind4 > length(x))) {
		if (ind4[1] > length(x)) ind4[1] <- length(x)
		if (ind4[2] > length(x)) ind4[2] <- length(x)
		if (ind4[3] > length(x)) ind4[3] <- length(x)
		if (ind4[4] > length(x)) ind4[4] <- length(x)
		cat("\n>>> Peak near the right end, see ?calcSN & inspect the results carefully\n")
		pR <- TRUE
		}
	
	# Windows overlap by one point but doing so avoid problems at edges
	
	if (pL) { # calc SN using only noise region on right
		indP <-	ind4[2]:ind4[3]
		indR <- ind4[3]:ind4[4]
		nR <- mean(x[indR])
		ans <- (max(x[indP])/nR)^2	
		
	} else { # calc SN using only noise region on left
		
		if (pR) {
			indL <- ind4[1]:ind4[2]
			indP <-	ind4[2]:ind4[3]
			nL <- mean(x[indL])
			ans <- (max(x[indP])/nL)^2	
			
		} else { # peak not at edge
			indL <- ind4[1]:ind4[2]
			indP <-	ind4[2]:ind4[3]
			indR <- ind4[3]:ind4[4]
			nL <- mean(x[indL])
			nR <- mean(x[indR])
			ans <- (max(x[indP])/mean(nL, nR))^2	
			}
		}
	ans
	}