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

	class(spectra) <- "Spectra"
	return(spectra) # spectra is now complete!
	}

