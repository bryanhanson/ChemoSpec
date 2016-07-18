
plotSpectra <- function(spectra, which = c(1),
	yrange = range(spectra$data),
	offset = 0.0, amplify = 1.0,
	lab.pos = mean(spectra$freq),
	showGrid = TRUE, ...) {
	
# Function to plot multiple spectra @ specified expansions & decorate
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, June 2009

	if (missing(spectra)) stop("No spectral data provided")
	chkSpectra(spectra)
	
	# set up and plot the first spectrum
	
	spectrum <- spectra$data[which[1],]*amplify

	plot(spectra$freq, spectrum, type = "n",
		xlab = spectra$unit[1], ylab = spectra$unit[2],
		ylim = yrange,
		frame.plot = FALSE, ...)
	if (showGrid) grid(ny = NA, lty = 1) # grid will be underneath all spectra
	lines(spectra$freq, spectrum, col = spectra$colors[which[1]], ...)
	lab.x <- lab.pos
	spec.index <- findInterval(lab.x, sort(spectra$freq))
	lab.y <- spectrum[spec.index]
	text(lab.x, lab.y, labels = spectra$names[which[1]], pos = 3, cex = 0.75)
	
	which <- which[-1] # first spectrum already plotted so remove it from the list
	count <- 0 # get the other spectra and plot them as well
	for(n in which) {
		count <- count + 1
		spectrum <- (spectra$data[n,]+(offset*count))*amplify
		points(spectra$freq, spectrum, type = "l", col = spectra$colors[n], ...)
		lab.y <- spectrum[spec.index]
		text(lab.x, lab.y, labels = spectra$names[n], pos = 3, cex = 0.75)
		}
	}

