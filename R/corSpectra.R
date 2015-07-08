

corSpectra <- function(spectra, plot = TRUE,
	limX = NULL, limY = NULL,
	nticks = 10, levels = NULL,
	pmode = "contour", drawGrid = TRUE,
	R = NULL, V = NULL, ...) {
	
# Function to carry out Nicholson's STOCSY analysis
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, December 2014

# For large data sets, there are extreme challenges with cor()
# and in the graphical display.

# NOTE: Cannot subset before computing cor() as this gives the wrong numerical answer

	if ((pmode == "contourplot") | (pmode == "levelplot")) {
		if (!requireNamespace("lattice", quietly = TRUE)) {
			stop("You need to install package lattice to use this option")
			}
		}
	
	if (missing(spectra)) stop("No spectral data provided")
	chkSpectra(spectra)

	# Check to see if spectra$freq is increasing - if not, findInterval will fail
	# Silently reverse things
	if (is.unsorted(spectra$freq)) {
		spectra$freq <- rev(spectra$freq)
		spectra$data <- spectra$data[,ncol(spectra$data):1]
		}
	
	if (is.null(R)) { # user did not provide pre-computed correlation matrix
		X <- spectra$data
		if (ncol(X) > 10000) message("Calculating cor() may take a moment or longer")
		R <- cor(X) # same as (t(X) %*% X)/(nrow(spectra$data) - 1)
		}
	
	if (is.null(V)) { # user did not provide pre-computed covariance matrix
		X <- spectra$data
		if (ncol(X) > 10000) message("Calculating cov() may take a moment or longer")
		V <- cov(X) # same as (t(X) %*% X)/(nrow(spectra$data) - 1)
		}

	if (ncol(R) > 8000) message("Graphical output will take some time with this many data points")
	
	# Helper function to compute ticks, labels & colors
	
	decor <- function(spectra, V, R, limX, limY, levels, pmode) {
				
		# For base functions, the default levels are different than for lattice
		# For base plots, the axes range from [0...1] (rgl too, at least for surface3d)
		# whereas for lattice, range is [1...ncol/nrow]
		# Hence limits must be expressed in different units in each case
		
		# 1.  Fix axis labels
		# If user gives limits in native units, must
		# translate into appropriate units to create labels
		
		# WARNING: labeling won't work when there is a gap
		
		if ((pmode == "contour") | (pmode == "image")) { # base functions
			LX <- c(0, 1) # default values
			LY <- c(0, 1)
			tickposX <- pretty(LX, n = nticks)
			ticklabX <- (diff(range(spectra$freq)) * tickposX) + min(spectra$freq)
			tickposY <- pretty(LY, n = nticks) 
			ticklabY <- (diff(range(spectra$freq)) * tickposY) + min(spectra$freq)

			if (!is.null(limX)) { # override when limX is given
				l <- findInterval(limX[1], spectra$freq)
				r <- findInterval(limX[2], spectra$freq)
				limX<- c(l, r)
				limX <- limX/ncol(R)
				tickposX <- pretty(limX, n = nticks)
				ticklabX <- (diff(range(spectra$freq)) * tickposX) + min(spectra$freq)
				LX <- limX
				}

			if (!is.null(limY)) { # override when limY is given
				l <- findInterval(limY[1], spectra$freq)
				r <- findInterval(limY[2], spectra$freq)
				limY<- c(l, r)
				limY <- limY/ncol(R)
				tickposY <- pretty(limY, n = nticks)
				ticklabY <- (diff(range(spectra$freq)) * tickposY) + min(spectra$freq)
				LY <- limY
				}
			}


		if ((pmode == "contourplot") | (pmode == "levelplot") | (pmode == "rgl")) { # lattice functions + rgl
			LX <- c(1, ncol(R)) # See notes above
			LY <- c(1, ncol(R))
			tickposX <- seq(LX[1], LX[2], length.out = nticks) 
			tickposX <- round(tickposX)
			ticklabX <- spectra$freq[tickposX]		
			tickposY <- seq(LY[1], LY[2], length.out = nticks) 
			tickposY <- round(tickposY)
			ticklabY <- spectra$freq[tickposY]
					
			if (!is.null(limX)) {
				l <- findInterval(limX[1], spectra$freq)
				r <- findInterval(limX[2], spectra$freq)
				limX <- c(l, r)
				tickposX <- seq(limX[1], limX[2], length.out = nticks) 
				tickposX <- round(tickposX)
				ticklabX <- spectra$freq[tickposX]		
				LX <- limX
				}

			if (!is.null(limY)) {
				l <- findInterval(limY[1], spectra$freq)
				r <- findInterval(limY[2], spectra$freq)
				limY <- c(l, r)
				tickposY <- seq(limY[1], limY[2], length.out = nticks) 
				tickposY <- round(tickposY)
				ticklabY <- spectra$freq[tickposY]		
				LY <- limY
				}
			}

		if (pmode == "exCon") {
			if (!requireNamespace("exCon", quietly = TRUE)) {
				stop("You need to install package exCon to use this plotting option")
				}
			LX <- range(spectra$freq)
			LY <- LX
			ticklabX <- NA
			ticklabY <- NA
			tickposX <- NA
			tickposY <- NA
					
			if (!is.null(limX)) {
				LX <- limX
				}

			if (!is.null(limY)) {
				LY <- limY
				}			
			
			}
			
		# 2.  Set levels (contours need a different default than images)
		#     Contours have one color for each level/break/cut
		#	  Image plots have n breaks and n-1 colors
		
		# Color scale for each level
		# blue/low -> red/high, anchored at zero (index 5, a shade of green)
		# max and min will come from the data (i.e., red will be at max of V)
		cscale <- c(rev(rainbow(4, start = 0.45, end = 0.66)), rev(rainbow(5, start = 0.0, end = 0.25)))
		# view with:
		# pie(rep(1, 9), col = cscale)
		
		refscale <- seq(-1, 1, length.out = 9)
				
		if ((pmode == "contour") | (pmode == "contourplot") | (pmode == "exCon")) {
			if (!requireNamespace("exCon", quietly = TRUE)) {
				stop("You need to install package exCon to use this plotting option")
				}
			if (is.null(levels)) {
				levels <- chooseLvls(M = R, n = 5L, mode = "even")
				msg <- paste("The levels chosen are:\n", paste(round(levels, 5), collapse = " "), sep = " ")
				message(msg)
				}	
			myc <- cscale[findInterval(levels, refscale)]
			}

		if ((pmode == "image") | (pmode == "levelplot")) {

			if (!is.null(levels)) {
				myc <- cscale[findInterval(levels, refscale)]
				# need to remove one color:
				nc <- length(myc)
				if ((nc %% 2) == 1) myc <- myc[-ceiling(nc/2)]
				if ((nc %% 2) == 0) myc <- myc[-floor(nc/2)]
				}

			if (is.null(levels)) { # must have one less color than breaks
				levels <- chooseLvls(M = R, n = 5L, mode = "even") # Gives 5 levels
				msg <- paste("The levels chosen are:\n", paste(round(levels, 5), collapse = " "), sep = " ")
				message(msg)
				myc <- cscale[c(1, 3, 7, 9)] # remove colors near zero
				}
			}
				
		if (pmode == "rgl") {
			# Levels don't apply here, simply assign color based upon value
			myc <- cscale[findInterval(R, refscale)] # the colors to be used/color selection			
			}
		
		# 3.  Labeling
		
		lab <- spectra$unit[1]
		if (lab == "ppm") {
			ticklabX <- as.character(round(ticklabX, 2))
			ticklabY <- as.character(round(ticklabY, 2))
			}
		if (lab == "wavenumber") {
			ticklabX <- as.character(round(ticklabX, 0))
			ticklabY <- as.character(round(ticklabY, 0))
			}
					
		L <- list(myc = myc, lab = lab, limX = LX, limY = LY,
			tickposX = tickposX, tickposY = tickposY,
			ticklabX = ticklabX, ticklabY = ticklabY,
			levels = levels,
			refscale = refscale, cscale = cscale)
			
		return(L)
		} # end of decor
	
	# Ready to plot
	
	if (plot) {
		d <- decor(spectra, V, R, limX, limY, levels, pmode)
		# Elements returned by decor:
		# 1. myc
		# 2. lab
		# 3. limX
		# 4. limY
		# 5. tickposX
		# 6. tickposY,
		# 7. ticklabX
		# 8. ticklabY,
		# 9. levels
		#10. reference scale
		#11. color scale
		
		# First two are lattice functions
		
		if (pmode == "levelplot") {
			p <- lattice::levelplot(R, xlab = d[[2]], ylab = d[[2]],
				col.regions = d[[1]],
				scales = list(
					x = list(at = d[[5]], labels = d[[7]]),
					y = list(at = d[[6]], labels = d[[8]])),
				xlim = d[[3]], ylim = d[[4]],
				at = d[[9]],
				colorkey = list( # fixed key, regardless of levels actually used
					at = d[[10]],
					col = d[[11]],
					labels = list(
					at = seq(-1.0, 1.0, by = 0.2), 
                         labels = as.character(seq(-1.0, 1.0, by = 0.2)))),
				...)
			print(p)
			}
		
		if (pmode == "contourplot") {
			p <- lattice::contourplot(R, xlab = d[[2]], ylab = d[[2]],
				col.regions = d[[1]],
				region = TRUE,
				scales = list(
					x = list(at = d[[5]], labels = d[[7]]),
					y = list(at = d[[6]], labels = d[[8]])),
				xlim = d[[3]], ylim = d[[4]],
				labels = FALSE,
				at = d[[9]], # passes through to panel function
				colorkey = list( # fixed key, regardless of levels actually used
					at = d[[10]],
					col = d[[11]],
					labels = list(
						at = seq(-1.0, 1.0, by = 0.2), 
                        labels = as.character(seq(-1.0, 1.0, by = 0.2)))),
				...)
			print(p)
			}

		# Next two are base functions (these are much faster)
		
		if (pmode == "image") {
			image(R, xlab = d[[2]], ylab = d[[2]],
				col = d[[1]], xlim = d[[3]], ylim = d[[4]],
				breaks = d[[9]],
				xaxt = "n", yaxt = "n", useRaster = TRUE, ...)			
			axis(1, at = d[[5]], labels = d[[7]])			
			axis(2, at = d[[6]], labels = d[[8]])			
			}

		if (pmode == "contour") {
			# Next 2 lines needed to get grid under contours
			plot(x = 0, y = 0, xlim = d[[3]], ylim = d[[4]], type = "n",
				xlab = "", ylab = "", xaxt = "n", yaxt = "n")
			if (drawGrid) abline(v = d[[6]], h = d[[5]], col = "gray95")		
			contour(R, xlab = d[[2]], ylab = d[[2]],
				xlim = d[[3]], ylim = d[[4]],
				col = d[[1]],
				levels = d[[9]],				
				drawlabels = FALSE,
				axes = FALSE, frame.plot = TRUE,
				xaxs = "i", yaxs = "i", add = TRUE, ...)
			axis(1, at = d[[5]], labels = d[[7]])			
			axis(2, at = d[[6]], labels = d[[8]])
			}
		
		# Interactive versions
		
		if (pmode == "exCon") {

			x1 <- findInterval(d[[3]][1], spectra$freq)
			x2 <- findInterval(d[[3]][2], spectra$freq)
			y1 <- findInterval(d[[4]][1], spectra$freq)
			y2 <- findInterval(d[[4]][2], spectra$freq)
			Z <- R[x1:x2, y1:y2]
					
			exCon::exCon2(M = Z, levels = d[[9]],
				x = seq(d[[3]][1], d[[3]][2], length.out = ncol(Z)),
				y = seq(d[[4]][1], d[[4]][2], length.out = nrow(Z)),
				, ...)		
			}

		if (pmode == "rgl") {
			x1 <- d[[3]][1]
			x2 <- d[[3]][2]
			y1 <- d[[4]][1]
			y2 <- d[[4]][2]
			Z <- R[x1:x2, y1:y2]

			# Crude attempt to adjust aspect ratio (max(Z) always 1 or -1)
			np <- nrow(Z)
			while(abs(max(Z)/np) < 0.40) {
				Z <- Z * 1.5	
				}

			open3d(windowRect = c(0, 0, 800, 800))
			surface3d(x = 1:np, y = 1:np, z = Z, color = d[[1]])
			}

		}
		
	L <- list(cov = V, cor = R)
	invisible(L)
	
	}
		
