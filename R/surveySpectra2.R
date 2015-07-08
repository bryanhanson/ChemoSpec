surveySpectra2 <-
function(spectra, method = c("sd", "sem", "sem95", "mad", "iqr"),
	lab.pos = 0.9*max(spectra$freq), ...)
{

# Function to visually inspect a Spectra object holistically.
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, May 2015

	chkSpectra(spectra)
	
	x <- spectra$freq
	M <- scale(spectra$data, scale = FALSE) # Center by column means
	
	# Compute the requested data
	
	if (method == "iqr"){
		y <- aaply(spectra$data, 2, seXyIqr)
		y <- y[,3] - y[,2]
		lab <- "iqr"
		}

	if (method == "sd") {
		y <- aaply(spectra$data, 2, sd)
		lab <- "sd"
		}

	if (method == "sem") {
		y <- aaply(spectra$data, 2, seXy)
		y <- y[,3] - y[,2]
		lab <- "sem"
		}

	if (method == "mad") {
		y <- aaply(spectra$data, 2, seXyMad)
		y <- y[,3] - y[,2]
		lab <- "mad"
		}

	if (method == "sem95") {
		y <- aaply(spectra$data, 2, seXy95)
		y <- y[,3] - y[,2]
		lab <- "sem95"
		}

	# Now set up the plot and plot it!
	
	M <- rbind(M, y)
	ymax <- max(M)
	
	# Offset the summary stat below everything else, with a small gap for aesthetics
	
	off1 <- diff(range(y))
	off2 <- min(M) - 0.05*diff(range(M))
	ymin <- off2 - off1
	off3 <- abs(min(M)) + abs(max(y)) + abs(0.05*diff(range(M)))
	
	plot(x, M[1,], type = "n", ylim = c(ymin, ymax), xlab = spectra$unit[1], ylab = "Centered Spectra", ...)
	for (i in 1:(nrow(M)-1)) {
		lines(x, M[i,], col = spectra$colors[i])
		}
	lines(x, M[nrow(M),] - off3)
	off4 <- min(M[nrow(M),]) - off3 + 0.5*diff(range(y))
	text(x = lab.pos, y = off4, labels = lab, cex = 1.2, adj = c(0.5, 1))
	}
