
cleanSTOCSYpeaks <- function(spectra, file = NULL, rowThres = 0.01, colThres = 0.01,
	collapseRows = TRUE, collapseCols = TRUE, verbose = TRUE) {

	message("Beta version of cleanSTOCSYpeaks, check your results carefully")
		
	# Step 0: Get the data
	
	pks <- read.csv(file, header = FALSE)
	names(pks)[1] <- "driver" # other columns will be V1, V2 ...
	driver <- NULL # NSE/CRAN checks
	pks <- plyr::arrange(pks, driver) # sort it
	
	# Step 1a: Remove any columns that are all NA
	
	throw <- NA_integer_
	
	if (verbose) message("Removing rows that are all NA")
	
	for (i in 1:nrow(pks)) {
		chk <- all(is.na(pks[i,2:ncol(pks)]))
		if (chk) throw <- c(throw, i)
		}
	
	throw <- throw[-1]
	if (verbose) cat("Removed ", length(throw), " rows\n")
	if (length(throw) > 0) pks <- pks[-throw,]
	
	# Step 1: Average & collapse any group of rows that are within the specified threshold

	if (collapseRows) {
		pks <- collapseRowsOrCols(pks, ind = 1, rows = TRUE, thres = rowThres, verbose = verbose)
		} # end of collapseRows = TRUE
		
	# Step 2: Align the peak positions with the known frequencies
	# Set up an empty matrix in which we'll align the peaks
	# But first, check the order of spectra$freq
	
	if (is.unsorted(spectra$freq)) spectra$freq <- rev(spectra$freq)

	if (verbose) message("Mapping frequencies onto spectra$freq")

	pks2 <- matrix(NA_real_, ncol = length(spectra$freq), nrow = nrow(pks))
	
	# Add a row with the frequency values, to be used later for diff calc
	
	pks2 <- rbind(spectra$freq, pks2)
	
	# dimnames(pks) <- list(
		# paste("D_", as.character(sprintf("%.3f", pks[,1])), sep = ""),
		# paste("F_", as.character(sprintf("%.3f", spectra$freq)), sep = ""))
	
	# Now fill the matrix
	
	for (i in 1:nrow(pks)) {
		for (j in 2:ncol(pks)) {
			if (is.na(pks[i,j])) next
			if (is.nan(pks[i,j])) next
			loc <- findInterval(pks[i,j], spectra$freq)
			pks2[i+1, loc] <- pks[i, j]
			}
		}
	
	# Step 3: Remove any columns that are all NA
	
	throw <- NA_integer_
	
	if (verbose) message("Removing columns that are all NA")
	
	for (i in 1:ncol(pks2)) {
		chk <- all(is.na(pks2[-1,i])) # avoiding reference row
		if (chk) throw <- c(throw, i)
		}
	
	throw <- throw[-1]

	if (length(throw) > 0) {
		pks2 <- pks2[,-throw]
		if (verbose) cat("Removed ", length(throw), " columns\n")
		}
		
	# Add back the driver peak info, which is not present in pks2
	# (needed as the source of collDiff in collapseRowOrCols)
	
	pks2 <- cbind(c(NA, pks[,1]), pks2)

	# Step 4: Repeat Step 1 except over the columns

	if (collapseCols) {
		pks2 <- collapseRowsOrCols(pks2, ind = 1, rows = FALSE, thres = colThres, verbose = verbose)
		}
	
	# Final clean up: convert NaN to NA (not sure where these come from)
	
	nan <- which(is.nan(pks2))
	pks2[nan] <- NA
	return(pks2[-1,]) # remove the reference row
	
	} # end of function

	
