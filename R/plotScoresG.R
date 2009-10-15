

plotScoresG <-
function(spectra, pca, pcs = c(1:3), scheme = "primary")
	{

# Function to plot PCA scores using ggobi
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Oct 2009

	chkSpectra(spectra)
	if (length(levels(spectra$groups)) > 8) stop("ggobi can only display 8 different groups")

	# set up the data frame properly labeled
	
	len <- length(pcs)
	data <- data.frame(pca$x[,pcs[1]])
	for (i in 2:len) data <- cbind(data, pca$x[,pcs[i]])

	eigensum <- sum(pca$sdev*pca$sdev) # prepare labels
	variance <- 100*(pca$sdev*pca$sdev/eigensum)
	pc.name <- c()
	for (i in 1:len) {
		pc.name[i] <- paste("PC", pcs[i], " (", format(variance[i], digits=2),
		"%", ")", sep = "")
		}
	names(data) <- pc.name

	# Fix color scheme: GGobi does not accept just any color scheme
	# Have to match existing color scheme to one ggobi can use.
	# Max number of groups is 8, we'll use only what we need.
	# Users probably want a consistent color scheme throughout,
	# so the note in getManyCsv about colors is important.
	
	# If not worried about consistency/mapping, users can just choose
	# either pastel or primary and get a plot.
	
	# If one has used GGobi colors from the outset, there is still the matter
	# of mapping the color to the number used inside GGobi
	# Test the colors used in spectra to see if it is already one of the
	# GGobi schemes.  If so, map it. If not, assign new colors.
	
	pri.names <- c("red3", "dodgerblue4", "forestgreen", "purple4",
		"orangered", "yellow", "orangered4", "violetred2")
	pas.names <- c("seagreen", "brown2", "skyblue2", "hotpink3",
		"chartreuse3", "darkgoldenrod2", "lightsalmon3", "gray48")	
	# Check to see if spectra was created using GGobi colors,
	# and if so, re-code as integer indices
	
	colors <- spectra$colors
	col.lev <- levels(as.factor(colors))	
	no.col <- length(col.lev)
	pri.ggobi <- TRUE
	pas.ggobi <- TRUE
	
	for (n in 1:no.col) {
		pri.ggobi <- col.lev[n] %in% pri.names
		if (!pri.ggobi) break		
		}
	
	for (n in 1:no.col) {
		pas.ggobi <- col.lev[n] %in% pas.names
		if (!pas.ggobi) break		
		}
	
	if (pri.ggobi) { # map pri.names to their corresponding index
			for (n in 1:no.col) {
			which <- grep(pri.names[n], colors)
			colors[which] <- n		
			}
		cat("Original color scheme from ", substitute(spectra), " was retained\n")
		}

	if (pas.ggobi) { # map pas.names to their corresponding index
			for (n in 1:no.col) {
			which <- grep(pas.names[n], colors)
			colors[which] <- n		
			}
		cat("Original color scheme from ", substitute(spectra), " was retained\n")
		}
		
	# If GGobi colors were not used originally, make up a scheme	
	if (!pas.ggobi && !pri.ggobi) {
		
		for (n in 1:no.col) {
			which <- grep(paste("^", col.lev[n], "$", sep = ""), colors)
			colors[which] <- n		
			}
			
		if (scheme == "primary") col.names <- pri.names
		if (scheme == "pastel") col.names <- pas.names

		cat("Colors have been mapped as follows:\n")	
		for (n in 1:no.col) {
			cat("Original Color: ", col.lev[n], "New Color: ", col.names[n], "\n")
			}
		}

	S1 <- ggobi(data)
	if (scheme == "primary") colorscheme(S1) <- "Set1 8"
	if (scheme == "pastel") colorscheme(S1) <- "Set2 8"
	glyph_color(S1[1]) <- colors

	}
