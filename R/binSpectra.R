binSpectra <-
function(spectra, bin.ratio) {

# Function to bin or bucket a Spectra object
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Nov 2009

	if (missing(spectra)) stop("No spectral data provided")
	if (missing(bin.ratio)) stop("No bin.ratio specified")
	if (!missing(bin.ratio)) if (bin.ratio < 1) stop("bin.ratio < 1.0 which implies data expansion!")
	chkSpectra(spectra)
	br <- bin.ratio
	
	# If there are gaps in the data, each data chunk must be separately binned,
	# then the whole re-assembled.
	
	chk <- check4Gaps(spectra$freq) # returns FALSE if no gaps

	if (length(chk) == 1) { # no gaps, proceed to binning
		bin <- binData(spectra$freq, spectra$data[1,], br) # bin x and get dim for y
		data <- matrix(NA, nrow = length(spectra$names), ncol = length(bin$sum.y))
		freq <- bin$mean.x

		for (n in 1:length(spectra$names)) { # bin each set of y data
			new <- binData(y = spectra$data[n,], bin.ratio = br)
			data[n,] <- new$sum.y
			}
		
		if (!dim(data)[2]*br == dim(spectra$data)[2]) { # report if data was chopped
			chop <- dim(spectra$data)[2] - dim(data)[2]*br
			cat("To preserve the requested bin.ratio, ", chop, " data point(s)\n")
			cat("has(have) been removed from the beginning of the data\n\n")
			# note: actual chopping occurred over in binData, called above
			}
			
		spectra$freq <- freq
		spectra$data <- data
		}		

	if (length(chk) > 1) { # there are gaps, bin each data chunk separately
		tmpfreq <- c()
		tmpdata <- matrix(nrow = length(spectra$names), ncol = 1)
		tot <- 0
		
		for (z in 1:nrow(chk)) {
			which <- chk[z,4]:chk[z,5]
			bin <- binData(spectra$freq[which],
				spectra$data[1,which], br) # bin x and get dim for y
			data <- matrix(nrow = length(spectra$names), ncol = length(bin$sum.y))
			freq <- bin$mean.x
			for (n in 1:length(spectra$names)) { # bin each set of y data
				new <- binData(y = spectra$data[n, which], bin.ratio = br)
				data[n,] <- new$sum.y
		

				}

			if (!dim(data)[2]*br == length(which)) { # report if data was chopped
				chop <- length(which) - dim(data)[2]*br
				cat("To preserve the requested bin.ratio, ", chop, " data point(s)\n")
				cat("has(have) been removed from the beginning of the data chunk", z, "\n\n")
				tot <- tot + chop
				# note: actual chopping occurred over in binData, called above
				}
			
			tmpfreq <- c(tmpfreq, freq)
			tmpdata <- cbind(tmpdata, data)
			}
			
		if (length(chk) > 1) cat("A total of", tot, "data points were removed to preserve the requested bin.ratio\n")
			
		spectra$freq <- tmpfreq
		spectra$data <- tmpdata[,-1]
		}		

	chkSpectra(spectra)
	spectra
	}		
