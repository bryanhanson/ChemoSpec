\name{splitSpectraGroups}
\alias{splitSpectraGroups}

\title{
Create New Groups from an Existing Spectra Object
}

\description{
This function takes an existing \code{"Spectra"} object and uses your instructions to split the existing \code{spectra$groups} into new groups.  The new groups are added to the existing \code{"Spectra"} object (a list) as new elements.  This allows one to use different combinations of factors than were originally encoded in the \code{"Spectra"} object.  The option also exists to replace the color scheme with one which corresponds to the new factors.
}

\usage{
splitSpectraGroups(spectra, inst = NULL, rep.cols = NULL, ...)
}

\arguments{
  \item{spectra}{An object of S3 class \code{"Spectra"}.}
  \item{inst}{A list giving the name of the new element to be created from a set of target strings given in a character vector.  See the example for the syntax.}
  \item{rep.cols}{Optional.  A vector giving new colors which correspond to the levels of \code{inst}.  Only possible if \code{inst} has only one element, as the possible combinations of levels and colors may get complicated.}

  \item{\dots}{Additional arguments to be passed downstream. Currently not used.}
}

\value{ An object of S3 class \code{"Spectra"}, modified to have additional elements as specified by \code{inst}.}

\details{
The items in the character vector are grepped among the existing \code{spectra$groups} entries; when found, they are placed in a new element of \code{spectra}.  In the example, all \code{spectra$groups} entries containing "G" are coded as "G" in a new element called \code{spectra$env}, and any entries containing "T" are handled likewise.  This amounts to a sort of recoding of factors (the example demonstrates this).  Every entry in \code{spectra$groups} should be matched by one of the entries in the character vector.  If not, you will get <NA> entries.  Also, if the targets in the character vector are not unique, your results will reflect the order of the levels.  Since this is a grep process, you can pass any valid grep string as the target.

If \code{rep.cols} is provided, these colors are mapped one for one onto the levels of the the first element of \code{inst}.  See the example for usage.  This provides a different means of changing the sample color encoding than \code{\link{conColScheme}}.
}

\references{\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\seealso{
\code{\link{conColScheme}}.
}

\examples{
data(CuticleIR)
# original factor encoding from when CuticleIR was created:
levels(CuticleIR$groups)
# split those original levels into 2 new ones (re-code them):
new.groups <- list(gen = c("G", "T"), trt = c("C", "E"))
new.CuticleIR <- splitSpectraGroups(CuticleIR, new.groups)
str(new.CuticleIR) # note two new elements, "gen" and "trt"
# split into one new factor and re-color:
new.groups <- list(gen = c("G", "T")) # only one element
new.CuticleIR <- splitSpectraGroups(CuticleIR, new.groups, rep.cols = c("pink", "orange"))
str(new.CuticleIR) # note one new element, "gen" and the colors have changed.

# note that if you want to use a newly created group in
# plotScores and other functions to drive the color scheme, you'll
# have to copy a new group to the groups element:
new.CuticleIR$groups <- new.CuticleIR$gen 
}

\keyword{ utilities }
