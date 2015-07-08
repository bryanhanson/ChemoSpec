

collapseRowsOrCols <- function(M, ind, rows = TRUE, thres, verbose = FALSE) {
	
	# Helper function to inspect a matrix, and collapse (average) either rows or columns whose
	# values are within the given threshold.  Collapsed rows are replaced by
	# their colMeans, collapsed columns by their rowMeans.
	# This has the effect of reducing one dimension of the matrix.
	# One row or column is used as the reference vector w/respect to the threshold.
	
	# Part of the ChemoSpec package
	# Bryan Hanson, DePauw University, June 2015
	
	# Helper function to do <= w/tolerance for machine precision
	# Modified from: http://stackoverflow.com/a/13331792/633251
	
	chk <- function(x, y) (isTRUE(all.equal(x, y)) | x < y + .Machine$double.eps^0.5)

	# The Main Event
	
	M <- as.matrix(M)
	
	if (rows) {
		if (is.unsorted(M[,ind])) stop("M[,ind] must be sorted")
		rD <- diff(M[,ind])
		
		if (verbose) {
			message("Collapsing rows")
			names(rD) <- paste(2:nrow(M), 1:(nrow(M)-1), sep = "-")
			cat("rowDiff is: \n")
			print(rD)
			}
			
		gather <- NA_integer_ # rows to average
		trash <- NA_integer_ # rows to trash later
	
		for (i in 1:length(rD)) {

			if (chk(rD[i],thres)) {
				gather <- c(gather, i)	# gather grows until a jump is found	
				if (i == length(rD)) { # We are at the end, wrap it up
					gather <- gather[-1]
					gather <- c(gather, gather[length(gather)] + 1) # add one onto the end
					if (verbose) cat("The following rows will be collapsed:", gather, "\n")
					M[gather[1],] <- colMeans(M[gather,], na.rm = TRUE)
					trash <- c(trash, gather[-1]) # discard these rows in a separate step
					}
				}

			if (!chk(rD[i],thres)) { # jump found, process what we have
				gather <- gather[-1]
				
				if (length(gather) == 0) { # only 1 value, no need to alter original
					gather <- NA_integer_ # reset
					next # nothing to do
					}		
				
				gather <- c(gather, gather[length(gather)] + 1) # add one onto the end
				
				if (verbose) cat("The following rows will be collapsed:", gather, "\n")
				
				M[gather[1],] <- colMeans(M[gather,], na.rm = TRUE)
				trash <- c(trash, gather[-1]) # discard later
				gather <- NA_integer_ # reset
				} # Done processing a set of rows w/i thres

			}
		
		# now clean it up
		
		trash <- trash[-1]	
		
		if (length(trash) > 0) {
			M <- M[-trash,]
			if (verbose) cat("Removed", length(trash), "rows (", trash, ")\n")
			}
			
	} # end of rows = TRUE


	if (!rows) {
		
		if (is.unsorted(M[ind,], na.rm = TRUE)) stop("M[ind,] must be sorted")
		
		cD <- diff(M[ind,])
		
		if (all(is.na(cD))) {
			message("No columns to collapse (colDiff all NA)")
			message("Set collapseCols to FALSE if calling from cleanSTOCSYpeaks")
			stop("Cannot collapse columns")
			}
			
		if (verbose) {
			message("Collapsing columns")
			names(cD) <- paste(2:ncol(M), 1:(ncol(M)-1), sep = "-")
			cat("colDiff is: \n")
			print(cD)
			}
			
		gather <- NA_integer_ # rows to average
		trash <- NA_integer_ # rows to trash later
	
		for (i in 1:length(cD)) {

			if (is.na(cD[i])) next
			
			if (chk(cD[i], thres)) {
				gather <- c(gather, i)	# gather grows until a jump is found	
				if (i == length(cD)) { # We are at the end, wrap it up
					gather <- gather[-1]
					gather <- c(gather, gather[length(gather)] + 1) # add one onto the end
					
					if (verbose) cat("The following columns will be collapsed:", gather, "\n")
					
					M[,gather[1]] <- rowMeans(M[,gather], na.rm = TRUE)
					trash <- c(trash, gather[-1]) # discard these rows in a separate step				
					}
				}

			if (!chk(cD[i], thres)) { # jump found, process what we have
				gather <- gather[-1]
				
				if (length(gather) == 0) { # only 1 value, no need to alter original
					gather <- NA_integer_ # reset
					next # nothing to do
					}		
				
				gather <- c(gather, gather[length(gather)] + 1) # add one onto the end
				
				if (verbose) cat("The following columns will be collapsed:", gather, "\n")
				
				M[,gather[1]] <- rowMeans(M[,gather], na.rm = TRUE)
				trash <- c(trash, gather[-1]) # discard later
				gather <- NA_integer_ # reset
				} # Done processing a set of rows w/i thres

			}
		
		# now clean it up
		
		trash <- trash[-1]	
		
		if (length(trash) > 0) {
			M <- M[,-trash]
			if (verbose) cat("Removed", length(trash), "columns\n")
			}
		
	} # end of rows = FALSE
	
	return(M)
	}
