\name{shrinkLeaf}
\alias{shrinkLeaf}
\title{
Shrink the Leaves of a Dendrogram}
\description{
This function shrinks the size of leaves of a dendrogram object.  The code was taken from the help files.  Not for end-user use.
}
\usage{
shrinkLeaf(n, spectra)
}
\arguments{
  \item{n}{
A node in a dendrogram object.
}
  \item{spectra}{
An object of S3 class \code{"Spectra"}.
}
}
\value{
Returns a node with the label size properties set.
}
\section{Called by}{\code{\link{hcaSpectra}}, \code{\link{hcaScores}}.}

\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\keyword{ utilities }
\keyword{ cluster }
