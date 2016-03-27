\name{SpectraObjectFromMatrix}
\Rdversion{1.1}
\title{
Creating Spectra Objects from Data Matrices
}
\alias{SpectraMatrix}

\description{
In \code{ChemoSpec}, spectral data sets are stored in an S3 class called \code{Spectra}, which contains a variety of information in addition to the spectra themselves.  \code{Spectra} objects are usually created by \code{\link{files2SpectraObject}}.  However, sometimes people have their data stored in a csv file with the frequencies in the first column, and the spectra samples in additional columns.  If the sample names are in the header row, it is pretty easy to create the \code{Spectra} object by hand, as shown in the example below.  Begin by looking at \code{\link{Spectra}} so you understand what it is you are trying to build.
}

\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
\dontrun{
	# Read in a matrix/data frame containing all the data.
	# Matrix should have the frequencies in the first column, samples
	# in additional columns.  The column headers should have the sample names.
	# Sample names should not have - or spaces in them.

	# Get the data matrix
	specs <- read.csv("Some Spectra.csv")

	# Set up the Spectra object
	mySpec <- list()
	mySpec$freq <- specs[,1] # frequencies in first column
	mySpec$data <- t(specs[,-1])
	mySpec$names <- names(specs[,-1]) # names from header row
	ns <- length(mySpec$names) # count the no. of samples

	# These next steps can be customized to great advantage.
	# If you know grep, you can use it on the names to
	# generate the next entries automatically
	mySpec$groups <- as.factor(rep("A", ns))
	mySpec$colors <- rep("black", ns)
	mySpec$sym <- rep(20, ns)
	mySpec$alt.sym <- rep("a", ns)

	# Final items
	mySpec$unit <- c("x unit", "y unit")
	mySpec$desc <- "Description of Samples"
	class(mySpec) <- "Spectra"

	library("ChemoSpec")
	chkSpectra(mySpec)

	str(mySpec) # Inspect if chkSpectra complains

	sumSpectra(mySpec) # Verify you got the groups etc correct

	# Plot to inspect
	plotSpectra(mySpec)

	# Check individual spectra for issues, use either
	loopThruSpectra(mySpec)
	# or
	plotSpectraJS(mySpec)
}

}

\keyword{ classes }
