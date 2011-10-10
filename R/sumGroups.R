sumGroups <-
function(spectra){
	
# Function to summarize groups in a Spectra object
# Part of ChemoSpec package
# Bryan Hanson, DePauw Univ, Dec 2009
	
	chkSpectra(spectra) # verify it's legit

	gr.l <- levels(spectra$group)
	count <- length(gr.l)
	g.sum <- data.frame(group = NA, no. = NA, color = NA,
			symbol = NA, alt.sym = NA)
	
	for (n in 1:count) {
		gi <- match(gr.l[n], spectra$groups) # find index 1st instance
		gr <- gr.l[n] # value of group
		no. <- length(which(gr == spectra$groups))
		col <- spectra$colors[gi] # value of color
		sym <- spectra$sym[gi] # value of symbol
		asym <- spectra$alt.sym[gi] # value of alt symbol
		g.sum <- rbind(g.sum, data.frame(group = gr, no. = no., color = col,
			symbol = sym, alt.sym = asym))
		}
	g.sum <- g.sum[-1,]
	g.sum <- subset(g.sum, no. > 0)
	rownames(g.sum) <- c(1:nrow(g.sum))
	g.sum
	}
