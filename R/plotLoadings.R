plotLoadings <-
function(spectra, pca, title = "no title provided",
	loads = c(1), ref = 1, ...) {
	
# Function to plot loadings vs. frequencies
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, June 2008
# This is the lattice version

	if (missing(spectra)) stop("No spectral data set provided")
	if (missing(pca)) stop("No PCA results provided")
	if (!class(spectra) == "Spectra") stop("Your spectral data set looks corrupt!")
	if (!("princomp" %in% class(pca) || "prcomp" %in% class(pca))) stop("Your pca results look corrupt!")
	
	title <- paste(title, ": Loadings Plot", sep = "")

	# Stack the requested data into a data frame for plotting
	
	names <- paste("PC", loads, "Loadings", sep = " ")
	names <- c("Reference Spectrum", names)
	x <- rep(spectra$freq, length(loads) + 1)
	
	z <- rep(names[1], length(spectra$freq))
	y <- spectra$data[ref,] # load in the reference spectrum
	
	for(n in 1:length(loads)) {
		y <- c(y, pca$rotation[,loads[n]]) # add in each loading
		z <- c(z, rep(names[n + 1], length(spectra$freq)))
		}

	z <- as.factor(z)
	z <- relevel(z, "Reference Spectrum")
	df <- data.frame(y, x, z) 
	
	# Do the plot
	# Note: no way exists to plot the x axis reversed for multiple panels
	
	trellis.par.set(par.sub.text = list(font = 1)) # makes subtitle plain text	
	p <- xyplot(y ~ x | z, data = df,
		main = title,
		xlab = spectra$unit[1], ylab = "", sub = pca$method,
		layout = c(1, length(loads) + 1),
		strip.left = TRUE, strip = FALSE, col = "black",
		scales = list(x = "same", y = "free"),
		panel = function(..., type = "h") {
			if (panel.number() == 1) {
				panel.xyplot(..., type = "l")
				} else {
					panel.xyplot(..., type = type)
					}
			}, ...)
		
	plot(p)
	}

