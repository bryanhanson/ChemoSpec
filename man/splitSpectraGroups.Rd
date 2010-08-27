\name{splitSpectraGroups}
\alias{splitSpectraGroups}

\title{
Create New Groups from an Existing Spectra Object
}

\description{
This function takes an existing \code{"Spectra"} object and uses your instructions to split the existing \code{spectra$groups} into new groups.  The new groups are added to the existing \code{"Spectra"} object as new elements.  These new elements can then be used with \code{\link{hypTestScores}} to carry out manova.  This allows one to use different combinations of factors than were originally encoded in the \code{"Spectra"} object.
}

\usage{
splitSpectraGroups(spectra, inst = NULL, ...)
}

\arguments{
  \item{spectra}{An object of S3 class \code{"Spectra"}.}
  \item{inst}{A list giving the name of the new element to be created from a set of target strings given in a character vector.  See the example for the syntax.}
  \item{\dots}{Additional arguments to be passed downstream. Currently not used.}
}

\value{ An object of S3 class \code{"Spectra"}, modified to have additional elements as specified by \code{inst}.}

\details{
The items in the character vector are grepped among the existing \code{spectra$groups} entries; when found, they are placed in a new element of \code{spectra}.  In the example, all \code{spectra$groups} entries containing "G" are coded as "G" in a new element called \code{spectra$env}, and any entries containing "T" are handled likewise.  This amounts to a sort of recoding of factors (the example demonstrates this).  Every entry in \code{spectra$groups} should be matched by one of the entries in the character vector.  If not, you will get <NA> entries.  Also, if the targets in the character vector are not unique, your results will reflect the order of the levels.  Since this is a grep process, you can pass any valid grep string as the target.
}

\references{\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\seealso{
One would likely use this function as a prelude to using \code{\link{hypTestScores}}.
}

\examples{
data(CuticleIR)
# original factor encoding from when CuticleIR was created:
levels(CuticleIR$groups)
# now split those original levels (re-code them):
new.groups <- list(env = c("G", "T"), trt = c("C", "E"))
new.CuticleIR <- splitSpectraGroups(CuticleIR, new.groups)
str(new.CuticleIR) # note two new elements, "env" and "trt"
}

\keyword{ utility }
