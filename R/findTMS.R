	
findTMS <- function (x, span, sn, thresh = 0.2, debug = FALSE) {
	
	# Part of ChemoSpec, December 2012, Bryan A. Hanson
		
	pp <- function (x, span) {
	# Borrowed from ChemometricsWithR by R. Wehrens
    span.width <- span * 2 + 1
    loc.max <- span.width + 1 - apply(embed(x, span.width), 1, 
        which.max)
    loc.max[loc.max == 1 | loc.max == span.width] <- NA
    pks <- loc.max + 0:(length(loc.max) - 1)
    unique(pks[!is.na(pks)])
		}

	if (debug) cat("\tfindTMS is looking around for TMS or TSP")
	
	peaks <- pp(x, span) # get all peaks by index in x, then calc SN
	thresh <- quantile(x > 0, thresh)
	keep <- which(x[peaks] >= thresh) # important to only look at large positive peaks
	peaks <- peaks[keep]
	
#	cat("Peak indices\n")
#	print(peaks)

	peaksSN <- rep(NA, length(peaks)) # pre-allocate for speed
	for (i in 1:length(peaks)) {
		peaksSN[i] <- calcSN(x, span, peaks[i])
		}
		
#	cat("Peak SN\n")
#	print(peaksSN)

	peaksSN <- which(peaksSN >= sn) # the large peaks by index
	
# 	cat("Above Threshold (orig index) = ", peaks[peaksSN], "\n")
# 	cat("Above Threshold (found pk index)= ", peaksSN, "\n")
	
	TMS <- peaks[peaksSN[length(peaksSN)]] # get the rightmost/last one which is TMS/TSP
	}
