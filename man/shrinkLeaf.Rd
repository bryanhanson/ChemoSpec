\name{shrinkLeaf}
\alias{shrinkLeaf}
\title{
Shrink the Leaves of a Dendrogram Based on a Spectra Object
}
\description{
This function shrinks the size of leaves of a dendrogram object.  The code was taken from the help files.  An internal function, not generally called by the user.
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

\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\keyword{ utilities }
\keyword{ cluster }
