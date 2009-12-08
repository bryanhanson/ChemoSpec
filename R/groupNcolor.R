groupNcolor <-
function(spectra, gr.crit = NULL, gr.cols = c("auto")) {

# Function to Process Group Assignments & Assign Colors
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Aug 2009
# Acts on all csv files in a directory.

	# use the group criteria (gr.crit) to classify the samples

	spectra$groups <- rep(NA, length(spectra$names)) # intialize
	
	files <- list.files(pattern = "\\.(csv|CSV)")
	files.noext <- substr(basename(files), 1, nchar(basename(files)) - 4)
	
	for (i in 1:length(gr.crit)) {
		which <- grep(gr.crit[i], files.noext)
		spectra$groups[which] <- gr.crit[i]
		}
	
	spectra$groups <- as.factor(spectra$groups)
	
	# assign each group a color for plotting later

	spectra$colors <- rep(NA, length(spectra$names)) # initialize
	
	if (identical(gr.cols[1], "auto")) {
		gr.cols <- brewer.pal(length(gr.crit), "Set1")
		for (i in 1:length(gr.crit)) {
			which <- grep(gr.crit[i], files)
			spectra$colors[which] <- gr.cols[i]
			}
	} else # match gr.cols with gr.crit & assign spectra$colors	
		for (i in 1:length(gr.crit)) {
			which <- grep(gr.crit[i], spectra$groups)
			spectra$colors[which] <- gr.cols[i]
			}

	# either way, associate symbols and alt.sym with each gr.crit
	sym.choice <- c(1, 2, 3, 15, 16, 17, 22, 8) # preferred symbols
	sym1 <- sym.choice[1:length(gr.crit)]
	sym2 <- letters[1:length(gr.crit)]
	if (length(gr.crit) > 8) {
		sym1 <- c(1:length(gr.crit))
		warning("Too many groups to use the preferred symbols!")
		}

	for (i in 1:length(gr.crit)) {
		which <- grep(gr.crit[i], spectra$groups)
		spectra$sym[which] <- sym1[i]
		spectra$alt.sym[which] <- sym2[i]
		}
	
	class(spectra) <- "Spectra"
	return(spectra) # spectra is now complete!
	}

