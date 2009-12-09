hcaScores <-
function(spectra, pca, title = "no title provided", scores = c(1:5),
method = "complete", use.sym = FALSE, ...) {

# Function to carry out HCA on PCA scores
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Aug 2009

	
	if (missing(spectra)) stop("No spectral data set provided")
	if (missing(pca)) stop("No pca results provided")
	if (!("princomp" %in% class(pca) || "prcomp" %in% class(pca))) stop("Your pca results look corrupt!")
	chkSpectra(spectra)

	if (use.sym) spectra$names <- paste(spectra$alt.sym, spectra$names, sep = " ")
	distance <- dist(as.data.frame(pca$x[,scores], row.names = spectra$names))

	title <- paste(title, ": HCA Analysis of PC Scores", sep = "")
	sub.title <- paste("Clustering method: ", method, sep = "")

	plotHCA(spectra = spectra, distance = distance, title = title, sub.title = sub.title,
		method = method, use.sym = use.sym)
	}

