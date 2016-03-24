
check4Gaps <- function(x, y = NULL, tol = 0.01, plot = FALSE, silent = FALSE, ...) {
	
# Function to inspect spectral data for gaps & record them
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, November 2009
	
	len.x <- length(x)
	p <- x[2] - x[1] # nominal freq/pt
	d1 <- c() # beg of data chunk by value
	d2 <- c() # end of data chunk by value
	d1i <- c() # beg of data chunk by index
	d2i <- c() # end of data chunk by index
	dend <- data.frame(d2 = NA, d2i = NA) # end of data chunks
	dbeg <- data.frame(d1 = NA, d1i = NA) # beginning of data chunks
		
	for (n in 1:(len.x - 1)) { # locate ends of data chunks
		t <- x[n + 1] - x[n] # WOULDN'T THIS BE BETTER WITH diff?
		if (!isTRUE(all.equal(t, p, tolerance = tol))) { # detects discontinuity
			dend <- rbind(dend, data.frame(d2 = x[n], d2i = n))
			dbeg <- rbind(dbeg, data.frame(d1 = x[n + 1], d1i = n + 1))
			}
		}

	chk <- dim(dend)[1]
	
	if (chk > 2) { # if 2 or more gaps were found, assemble a table of the data chunks
		dend <- dend[-1,] # clean out NAs
		dbeg <- dbeg[-1,]
		tmp <- data.frame(beg.freq = NA, end.freq = NA, size = NA, beg.indx = NA, end.indx = NA)
		first <- data.frame(beg.freq = x[1], end.freq = dend[1,1], size = NA,
			beg.indx = 1, end.indx = dend[1,2])
		last <- data.frame(beg.freq = dbeg[nrow(dbeg),1], end.freq = x[length(x)], size = NA,
			beg.indx = dbeg[nrow(dbeg),2], end.indx = length(x))
	
		for (n in 1:(nrow(dbeg)-1)) {	
			tmp <- rbind(tmp, data.frame(beg.freq = dbeg[n,1], end.freq = dend[n+1, 1],
				size = NA, beg.indx = dbeg[n,2], end.indx = dend[n+1, 2]))
			}

		tmp <- tmp[-1,] # clean out NAs
		df <- rbind(first, tmp, last)
		df[,3] <- abs(df[,2] - df[,1])
		
		}

	if (chk == 2) { # if 1 gap was found, assemble a table of the data chunks
		dend <- dend[-1,] # clean out NAs
		dbeg <- dbeg[-1,]
		first <- data.frame(beg.freq = x[1], end.freq = dend[1,1], size = NA,
			beg.indx = 1, end.indx = dend[1,2])
		last <- data.frame(beg.freq = dbeg[nrow(dbeg),1], end.freq = x[length(x)], size = NA,
			beg.indx = dbeg[nrow(dbeg),2], end.indx = length(x))
	
		df <- rbind(first, last)
		df[,3] <- abs(df[,2] - df[,1])
		
		}
		
	
	if ((chk == 1) && (!silent)) cat("No gaps were found by check4Gaps\nNo plot will be made\n")
	if (chk == 1)  df <- FALSE
	
	if ((chk > 1) && (plot)) {
		if (missing(y)) stop("No y values provided; cannot plot!")
		plot(x, y, type = "l", col = "black", main = "Gaps in Frequency Data",
			ylab = "", xlab = "marked regions are skipped in data set", ...)
		ybottom <- min(y) - 0.1 * diff(range(y))
		ytop <- max(y) + 0.1* diff(range(y)) # only needs to exceed plotting area to fill it
		for (n in 1:(nrow(df)-1)){
			lines(x = c(df[n,2], df[n+1,1]), y = c(y[df[n,5]], y[df[n+1,4]]), lty = 2, col = "white")
			rect(xleft = df[n,2]+p, ybottom, xright = df[n+1,1]-p, ytop, density = 15, col = "pink")
			}
		}

	df
	}
